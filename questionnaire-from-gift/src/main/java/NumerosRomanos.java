import java.util.*;

/**
 * Created by alvaro on 18/06/15.
 */
public class NumerosRomanos {

    private static SortedMap<Integer, String> _cantidades;

    public static SortedMap<Integer,String> getCantidades(){
        if (_cantidades == null) {
            _cantidades = inicializaCantidades();
        }
        return _cantidades;
    }

    private static SortedMap<Integer,String> inicializaCantidades() {
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

        SortedMap<Integer, String> ret = new TreeMap<Integer, String>( new Comparator<Integer>(){
            @Override
            public int compare(Integer o1, Integer o2) {
                return o2 - o1;
            }
        });

        for( int i : map.keySet() ){
            ret.put( i, map.get(i) );
            ret.put( i*1000, map.get(i).toUpperCase() );
        }
        return ret;
    }


    static String aRomano(int n) {
        log( "" + n );
        if( n == 0 ){
            return "";
        }
        int i = numeroMasGrandeMenorQue(n);
        log( "  " + i + "->" + _cantidades.get(i) );
        return getCantidades().get(i) + aRomano( n - i );
    }

    private static void log(String s) {
        System.out.println(s);
    }

    private static int numeroMasGrandeMenorQue(int n) {
        for( int i : getCantidades().keySet() ){
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
