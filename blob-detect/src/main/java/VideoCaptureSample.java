

import com.github.sarxos.webcam.Webcam;

import javax.swing.*;
import java.util.*;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;
import java.io.Closeable;
import java.lang.reflect.InvocationTargetException;

/**
 * Created with IntelliJ IDEA.
 * User: alvaro
 * Date: 26/12/13
 * Time: 16:00
 * To change this template use File | Settings | File Templates.
 */
public class VideoCaptureSample implements Closeable{

    private Webcam _webcam;

    public interface ImageCapturedListener {
        void imageCaptured(BufferedImage i);
    }
    
    private Dimension _dimension;
    
    public VideoCaptureSample(){
        _dimension = null;
    }
    
    public VideoCaptureSample(int w, int h){
        _dimension = new Dimension(w,h);
    }

    public  VideoCaptureThread startVideoCapture(ImageCapturedListener l){
        VideoCaptureThread ret = new VideoCaptureThread(l);
        ret.start();
        return ret;
    }

    private static int _seq = 0;

    private class VideoCaptureThread extends Thread{
        private final ImageCapturedListener _listener;
        private boolean _stopASAP;

        public VideoCaptureThread( ImageCapturedListener listener ){
            _listener = listener;
            setName( "VideoCaptureThread" + _seq );
            setPriority(Thread.MIN_PRIORITY);
            _seq++;
        }

        public void stopASAP(){
            log( "please stop ASAP");
            _stopASAP = true;
        }

        @Override
        public void run() {
            while( !_stopASAP ){
                BufferedImage i = captureImage();
                _listener.imageCaptured(i);
            }
            log( "end of run");
        }
    }

    private static void log(String s) {
        //System.out.println( s );
    }

    public Webcam webcam(){
        if( _webcam == null ){
            _webcam = Webcam.getDefault();
            Dimension d = _dimension;
            if( d == null ){
                Dimension[] dimensions = _webcam.getViewSizes();
                _webcam.getCustomViewSizes();
                Arrays.sort( dimensions, new Comparator<Dimension>(){
                    @Override
                    public int compare( Dimension d1, Dimension d2){
                        return d2.height * d2.width - d1.height * d1.width;
                    }
                });
                System.out.println( "******" + Arrays.asList(dimensions) );
                d = dimensions[0];
            }
            d = new Dimension(1280,1024);
            _webcam.setCustomViewSizes( new Dimension[]{d} );
            _webcam.setViewSize(d);
            System.out.println("Camera size:" + _webcam.getViewSize());
            _webcam.open();
            System.out.println("Camera size:" + _webcam.getViewSize());
        }
        return _webcam;
    }

    public BufferedImage captureImage(){
        return webcam().getImage();
    }

    @Override
    public void close(){
        if( _webcam != null ){
            _webcam.close();
        }
        _webcam = null;
    }

    public static void main( String[] args ){
        final JFrame frame = new JFrame( "Video" );

        final VideoCaptureSample nvc = new VideoCaptureSample();
        ImageCapturedListener listener = new ImageCapturedListener(){
            @Override
            public void imageCaptured(final BufferedImage i) {
                try {
                    SwingUtilities.invokeAndWait(new Runnable() {
                        @Override
                        public void run() {
                            Container p = frame.getContentPane();
                            Dimension size = p.getSize();
                            Graphics g2d = p.getGraphics();
                            g2d.drawImage(i, 0, 0, size.width, size.height, null);
                            g2d.setColor(Color.pink);
                            g2d.drawLine(0,0,size.width,size.height);
                            g2d.drawString( String.format("%dx%d", i.getWidth(),i.getHeight() ), 10, size.height-10 );
                            g2d.dispose();
                            log("image drawn");
                        }
                    });
                } catch (InterruptedException e) {
                    e.printStackTrace();
                } catch (InvocationTargetException e) {
                    e.printStackTrace();
                }
            }
        };

        frame.setSize(200,200);
        frame.setVisible(true);
        final VideoCaptureThread vct = nvc.startVideoCapture(listener);

        frame.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                vct.stopASAP();
                frame.setVisible(false);
                frame.dispose();
            }

            @Override
            public void windowClosed(WindowEvent e) {
                nvc.close();
                System.exit(0);
            }
        });
    }
}