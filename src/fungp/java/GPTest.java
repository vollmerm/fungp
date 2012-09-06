/* GPTest.java
 *
 * This is a very simple example use of the fungp Java API. It should be mostly
 * self explanatory.
 *
 * The Java API for fungp is very primitive. To approximate function pointers it
 * uses strings of static method names, rather than something like anonymous
 * classes.
 */
package fungp;

import java.util.HashMap;
import fungp.GPSearch;
import clojure.lang.ISeq;
import clojure.lang.IFn;

public class GPTest {

    public static Object test() {
        HashMap<String, Object> hmap = new HashMap<String, Object>();

        String[] terminals = {"x"};
        String[] functions = {"+ 2", "- 2", "* 2"};
        String[] javaImports = {"fungp.GPTest"};
        Integer[] numbers = {1, 2, 3, 4, 5};

        hmap.put("iterations", 5);
        hmap.put("migrations", 10);
        hmap.put("num-islands", 2);
        hmap.put("population-size", 25);
        hmap.put("max-depth", 10);
        hmap.put("terminals", terminals);
        hmap.put("functions", functions);
        hmap.put("numbers", numbers);
        hmap.put("fitness", "fungp.GPTest/fitnessTest");
        hmap.put("report", "fungp.util/generic-report");
        hmap.put("java-imports",javaImports);
        hmap.put("mutation-depth", 3);

        return GPSearch.runSearch(hmap);
    }

    public static long fitnessTest(Object tree) {
        Object tmp;
        long error = 0;
        int[] args = new int[1];
        String[] terminals = {"x"};
        IFn compiledTree = (IFn) GPSearch.compileTree(tree, terminals);
        try {
          for (args[0] = 0; args[0] < 10; args[0]++) {
              tmp = GPSearch.applyTree(compiledTree, args);
              if (tmp instanceof Long) {
                error += Math.abs((long) tmp - (args[0] * args[0] + (args[0] * 5)));
              } else if (tmp instanceof Integer) {
                error += Math.abs((int) tmp - (args[0] * args[0] + (args[0] * 5)));
              } else { throw new Exception("Unknown number type."); }
          }
        } catch (Exception e) { System.out.println(e); }
        return error;
    }
}
