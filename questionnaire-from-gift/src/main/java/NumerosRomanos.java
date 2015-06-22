import java.util.*;

/**
 * Created by alvaro on 18/06/15.
 */
public class NumerosRomanos {

    private static SortedMap<Integer, String> _cantidades = new TreeMap<Integer, String>( new Comparator<Integer>(){
        @Override
        public int compare(Integer o1, Integer o2) {
            return o2 - o1;
        }
    });

    static {
        inicializaDatos();
    }

    private static void inicializaDatos() {
        Map<Integer,String> map = new HashMap<Integer,String>();
        map.put(1, "i");
        map.put(4, "iv");
        map.put(9, "ix");
        map.put(10, "x");
        map.put(29,"ix");
        map.put(40, "xl");
        map.put(50, "l");
        map.put(90, "xc");
        map.put(100, "c");
        map.put(400, "cd");
        map.put(500, "d");
        map.put(900, "cm");
        map.put(1000, "m");

        for( int i : map.keySet() ){
            _cantidades.put( i, map.get(i) );
            _cantidades.put( i*1000, map.get(i).toUpperCase() );
        }
    }


    static String aRomano(int n) {
        log( "" + n );
        if( n == 0 ){
            return "";
        }
        int i = numeroMasGrandeMenorQue(n);
        log( "  " + i + "->" + _cantidades.get(i) );
        return _cantidades.get(i) + aRomano( n - i );
    }

    private static void log(String s) {
        System.out.println(s);
    }

    private static int numeroMasGrandeMenorQue(int n) {
        for( int i : _cantidades.keySet() ){
            if( i <= n ){
                return i;
            }
        }
        throw new IllegalStateException( "No puede ser que llege aquí, tiene que encontrar un número adecuado");
    }


    public static void main(String args[]) {
        System.out.println( aRomano(99) );
    }
}
