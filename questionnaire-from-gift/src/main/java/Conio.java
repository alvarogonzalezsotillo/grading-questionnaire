import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;

/**
 * Reemplazo para la libreria CONIO de Borland en entornos Windows.
 * Permite la utilizacion de las funciones de localizacion en pantalla
 * y colores, basados en la utilizacion de secuencias de escape ANSI.
 * Estas secuencias son reconocidas en Windows ME y anterior, Linux y 
 * la mayoria de terminales *NIX.
 * 
 * Ademas permite la lectura de una linea por teclado.
 * 
 * @author alvaro.gonzalezsotillo@educa.madrid.org
 *
 */
public class Conio{

	/**
	 * Construye un objeto con la entrada y salida estandar del proceso
	 */
	public Conio(){
		this( System.out, System.in);
	}
	
	/**
	 * Construye un nuevo objeto con la salida y entrada especificada
	 * @param out salida estandar
	 * @param in entrada estandar
	 */
	public Conio(PrintStream out, InputStream in) {
		this.out = out;
		this.in = in;
	}

	private PrintStream out;
	private InputStream in;

	/* Defino los colores que puedo utilizar */
	public static final int BLACK   =0;
	public static final int RED     =1;
	public static final int GREEN   =2;
	public static final int YELLOW  =3;
	public static final int BLUE    =4;
	public static final int MAGENTA =5;
	public static final int CYAN    =6;
	public static final int WHITE   =7;

	/* Defino algunos de los atributos posibles */
	public static final int RESET     =0;
	public static final int BRIGHT    =1;
	public static final int DIM       =2;
	public static final int UNDERLINE =3;
	public static final int BLINK     =4;
	public static final int REVERSE   =7;
	public static final int HIDDEN    =8;

	private static final String ESCAPE = "\u001b[";
	
	/**
	 * Utilidad para la salida formateada, tipo printf en C
	 * @param format Formato de la impresion
	 * @param args argumentos de printf
	 */
	public void printf( String format, Object... args){
		out.printf( format, args );
	}
	
	/**
	 * Coloca el cursor en la posicion especificada, siendo el 1,1 la
	 * posicion superior izquierda
	 * @param x columna
	 * @param y fila
	 */
	public void gotoxy(int x, int y){
		printf( ESCAPE + "%d;%dH", y, x );
	}
	
	/**
	 *  Borra la pantalla
	 */
	public void clrscr(){
		printf( ESCAPE + "2J" );
	}

	/**
	 * Borra la linea actual
	 */
	public void clrline(){
		printf ( ESCAPE + "2K" );
	}
	
	/**
	 * Cambia el color de fondo y primer plano del caracter bajo
	 * el cursor y de los proximos caracteres a imprimir
	 * 
	 * @param color Color de los caracteres
	 * @param fondo Color del fondo
	 */
	public void color( int color, int fondo ){
		printf ( ESCAPE + "%d;%dm", (color) + 30, (fondo) + 40  );
	}
	
	/**
	 * Cambia los atributos del caracter bajo el cursos y de los  proximos caracteres
	 * a imprimir
	 * 
	 * @param x
	 */
	public void attr( int x){
		printf ( ESCAPE + "%dm", (x) );
	}

	/**
	 * Lee una linea de la entrada definida en el objeto. No incluye el retorno de linea
	 * @return La linea leida
	 */
	public String gets() {
		BufferedReader br = new BufferedReader( new InputStreamReader(in) );
		try {
			return br.readLine();
		}
		catch (IOException e) {
			throw new RuntimeException( e );
		}
	}

	public static void main( String args[] ){
		Conio c = new Conio();
		c.clrscr();
		c.color(Conio.GREEN, Conio.RED);
		c.gotoxy(10, 10);
		c.printf("Jaime");
		c.color(Conio.WHITE, Conio.BLACK);
		c.gotoxy( 1, 20 );


	}
	
	
}