package gui

import java.awt.event._
import java.awt.image.BufferedImage
import java.awt.{BorderLayout, Component, GridLayout, Image}
import java.io.File
import java.util
import javax.imageio.ImageIO
import javax.swing._
import javax.swing.border.TitledBorder

import imgproc.ocr.DefaultTrainedOneLetterOCR
import org.opencv.imgproc.Imgproc

import scala.util.Try

object Classifier extends App{

  def log(a: Any) = println(a)

  val baseDir = new File("/home/alvaro/github/grading-questionnaire/build/test-img/ExtractLettersApp")

  case class PatternImage(file:File){
    val image : BufferedImage = {
      val original = ImageIO.read(file)
      val height = 25
      val width = original.getWidth*25/original.getHeight
      val image = new BufferedImage(width,height,BufferedImage.TYPE_INT_RGB)
      image.getGraphics.drawImage(original,0,0,width,height,null)
      image
    }

    val icon = new ImageIcon(image)
    def moveToDir( dir: File ) = {
      log( s"moveToDir:$dir")
      val newFile = new File( dir, file.getName)
      log( s"  newFile:$newFile")
      assert( !newFile.exists() )
      val dirCreated = newFile.getParentFile.mkdirs()
      log( s"  dirCreated: $dirCreated ${newFile.getParentFile}" )
      val success = file.renameTo(newFile)
      assert( success )
      PatternImage(newFile)
    }
  }

  class ImagesModel(val dir:File, val char: Char) extends DefaultListModel[PatternImage]{
    def add( images: Seq[PatternImage] ) = images.foreach(addElement)
    def remove( images: Seq[PatternImage] ) : Unit = images.foreach(removeElement)
    def readFromDir = add( patternImagesOfDir(dir) )
    def allImages : Seq[PatternImage] = {
      val images = new Array[Object](getSize)
      copyInto(images)
      images.toSeq.asInstanceOf[Seq[PatternImage]]
    }

  }


  object ImagesRenderer extends ListCellRenderer[PatternImage] {

    val peerRenderer: ListCellRenderer[Icon] = (new DefaultListCellRenderer).asInstanceOf[ListCellRenderer[Icon]]

    override def getListCellRendererComponent (list: JList[_ <: PatternImage], task: PatternImage, index: Int, isSelected: Boolean, cellHasFocus: Boolean): Component = {

      peerRenderer.getListCellRendererComponent(list.asInstanceOf[JList[Icon]], task.icon, index, isSelected, cellHasFocus)
    }
  }

  private def patternImagesOfDir( dir: File ) : Seq[PatternImage] = {
    val files = dir.listFiles

    if( files == null )
      Seq()
    else
      files.map( f => Try(PatternImage(f)) ).
        filter( _.isSuccess ).
        map(_.get)
  }

  abstract class ImagesList(dir: File, char: Char) extends JList[PatternImage]{
    setModel( new ImagesModel(dir,char) )
    setCellRenderer(ImagesRenderer)
    def imagesModel = getModel.asInstanceOf[ImagesModel]
  }

  object ImagesList{

    val letterToList = scala.collection.mutable.Map[Char,ImagesList]().withDefaultValue(null)

    val keyListener = new KeyAdapter {
      override def keyPressed(e: KeyEvent) : Unit = {
        log( s"KeyPressed:$e")
        val char = e.getKeyChar
        val destinationList = ImagesList.letterToList(char)
        if( destinationList == null ){
          return
        }
        val sourceList = e.getSource.asInstanceOf[ImagesList]
        val indices = sourceList.getSelectedIndices
        if( indices.isEmpty ){
          return
        }
        val images = indices.map( sourceList.imagesModel.get )

        destinationList.imagesModel.add(images.map(_.moveToDir(destinationList.imagesModel.dir)))
        sourceList.imagesModel.remove(images)
      }
    }

    for( c <- Seq( 'A','B','C','D') ){
      val dir = new File(baseDir, s"uppercase-${c.toLower}" )
      val list = new ImagesList(dir, c) {}
      list.addKeyListener(keyListener)
      letterToList(c) = list
    }
    letterToList(' ') = new ImagesList(baseDir,' '){}
    letterToList(' ').addKeyListener(keyListener)
  }

  object ListsPanel extends JPanel{
    val letters = ImagesList.letterToList.keys.toArray.sorted
    setLayout(new GridLayout(1, letters.size))
    for( l <- letters ) {
      val list = ImagesList.letterToList(l)
      list.imagesModel.readFromDir
      val c = new JScrollPane( list )
      c.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS)
      val title = if( l == ' ' ) "Sin clasificar" else s"Letra $l"
      val border = new TitledBorder( title )
      c.setBorder(border)
      add(c)
    }
  }

  object ClassifyButton extends JButton("Clasificar automáticamente los que aún no estén clasificados"){
    val listener = new ActionListener{
      override def actionPerformed(e: ActionEvent): Unit = {
        val ocr = DefaultTrainedOneLetterOCR
        val undefinedList = ImagesList.letterToList(' ')
        val images = undefinedList.imagesModel.allImages
        for( i <- images ) {
          val mat = imgproc.Implicits.BufferedImage2Mat(i.image)
          log( s"Clasificando:${i.file}")
          ocr.predict(mat).prediction match{
            case Some(c) =>
              log( s"  ${i.file} clasificado como: $c")
              undefinedList.imagesModel.removeElement(i)
              val destModel = ImagesList.letterToList(c).imagesModel
              destModel.addElement(i.moveToDir(destModel.dir))
            case None =>
              log( s"  ${i.file} clasificado sin clasificar")
          }
        }
      }
    }
    addActionListener(listener)
  }

  import javax.swing.UIManager

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  val frame = new JFrame("Clasificador")
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.add(ListsPanel, BorderLayout.CENTER)
  frame.add(ClassifyButton,BorderLayout.SOUTH)
  frame.pack
  frame.setVisible( true )
  nu.pattern.OpenCV.loadLibrary()



}
