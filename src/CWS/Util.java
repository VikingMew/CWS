package CWS;

/**
 * Created with IntelliJ IDEA.
 * User: vikingmew
 * Date: 12/26/12
 * Time: 4:13 PM
 */
public class Util {
    public static double[][] doubleouter(double[]a,double[] b) {
         double[][] martix = new double[a.length][b.length];
         for(int i = 0; i < a.length;i++) {
             for(int j = 0;j<b.length;j++) {
                 martix[i][j] = a[i]*b[j];
             }
         }
        return martix;
    }
    public static void main(String[] args) {
        double[] a = {1,2,3};
        double[] b = {4, 5, 6};
        System.out.println(doubleouter(a,b));
    }
}
