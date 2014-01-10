import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class PrimeSetsGen {

	// The number for which the sequence to be generated
	int nmax;

	// If true write to log file( in the name defined above), if false print to
	// the console
	boolean outToFile;

	// Maximum Sequences to be generated
	int outLimit;

	// File Writer
	BufferedWriter fw = null;

	// Counter to keep track of output
	int outCount;

	// Date formatter - 2013-12-21 04:55:22:018
	Date dNow = new Date();
	static SimpleDateFormat ft = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss:SSS");

	// Map to store all prime-sum pairs
	Map<Integer, List<Integer>> primeAdderMap = new HashMap<Integer, List<Integer>>();

	// Start time in milliseconds
	static long startTime = System.currentTimeMillis();

	/*
	 * Ctor
	 */
	PrimeSetsGen(int nmax, boolean outToFile, int outLimit) {
		this.nmax = nmax;
		this.outToFile = outToFile;
		this.outLimit = outLimit;
		initFileWriter();
	}

	/*
	 * Main method
	 * 
	 * Initiates the chainWalker logic for each valid prime-sum-pair which is
	 * loaded using method loadPrimeAdderMap
	 * 
	 * @see loadPrimeAdderMap
	 */
	public static void main(String[] args) {
		startTime = System.currentTimeMillis();
		PrimeSetsGen primeSetsGen = initCmdArgs(args);
		primeSetsGen.loadPrimeAdderMap();
		for (int n = 1; n <= primeSetsGen.nmax; n++) {
			List<Integer> start = new ArrayList<Integer>();
			start.add(n);
			primeSetsGen.genValidSeq(start);
		}
		primeSetsGen.endProgram();
	}

	/************************************************************/
	/* BEGIN: CORE LOGIC FOR FINDING VALID SEQUENCES */
	/************************************************************/

	/*
	 * Returns true if a number is prime
	 */
	boolean isPrime(double n) {
		int nsqrt = (int) Math.sqrt(n);
		for (int factor = 2; factor <= nsqrt; factor++) {
			if (n % factor == 0) {
				return false;
			}
		}
		return true;
	}

	/*
	 * Loads all int-pairs which add to a prime sum for every int from 1 to nmax
	 * e.g., for nmax=4, it's {1=[2, 4], 2=[1, 3, 5], 3=[2, 4], 4=[1, 3], 5=[2]}
	 */
	void loadPrimeAdderMap() {
		for (int i = 1; i <= nmax; i++) {
			for (int j = 1; j <= nmax; j++) {
				if (i != j && isPrime(i + j)) {
					List<Integer> primePairs = primeAdderMap.get(i);
					if (primePairs == null) {
						primePairs = new ArrayList<Integer>();
					}
					primePairs.add(j);
					primeAdderMap.put(i, primePairs);
				}
			}
		}
	}

	/*
	 * Returns arrangements of digits 1:n using the primeAdder pairs
	 * 
	 * Algorithm used: Recursive Logic which 'stiches' the integers by walking
	 * the primeAdder Map. For example, consider the following map for nmax=5
	 * 
	 * {1=[2, 4], 2=[1, 3, 5], 3=[2, 4], 4=[1, 3], 5=[2]}
	 */
	void genValidSeq(List<Integer> startChain) {
		if (startChain.size() == nmax) {
			// Excellent - we've hit a jackpot
			outputSeq(startChain);
			return;
		}
		// Get the last link (int) in the chain
		int lastLink = startChain.get(startChain.size() - 1);
		// Look up possible traversal paths for this last link
		List<Integer> primeAdders = primeAdderMap.get(lastLink);

		if (primeAdders == null || primeAdders.size() == 0) {
			return;
		} else {
			// Remove nodes already traversed as they cannot be reused
			List<Integer> newPrimeAdders = new ArrayList<Integer>(primeAdders);
			newPrimeAdders.removeAll(startChain);
			// For each possible traversal path possible...
			for (int primeAdder : newPrimeAdders) {
				List<Integer> newChain = new ArrayList<Integer>(startChain);
				// Add next link to the chain
				newChain.add(primeAdder);
				// Recursively call the same method to find the end of chain
				genValidSeq(newChain);
			}
		}
	}

	/************************************************************/
	/* END: CORE LOGIC FOR FINDING VALID SEQUENCES */
	/************************************************************/

	/************************************************************/
	/* BEGIN: UTILITY AND STANDARD METHODS FOR INIT/EXIT/IO */
	/************************************************************/

	/*
	 * Initializes file writer
	 */
	void initFileWriter() {
		if (outToFile) {
			try {
				fw = new BufferedWriter(new FileWriter(new File(
						"144813_primeseq_" + nmax + ".log")));
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	/*
	 * Writes the output to file/console
	 */
	private void outputSeq(List<Integer> startChain) {
		outCount++;
		if (outLimit > 0 && outCount > outLimit) {
			endProgram();
		}
		String seqStr = startChain.toString();
		seqStr = seqStr.substring(1, seqStr.length() - 1);

		String outStr = outCount + ";" + seqStr + ";" + ft.format(new Date());
		if (outToFile) {
			try {
				fw.write(outStr + System.getProperty("line.separator"));
			} catch (IOException e) {
				e.printStackTrace();
			}
		} else {
			System.out.println(outStr);
		}
	}

	/*
	 * Uses command line args to initialize the generator class
	 */
	static PrimeSetsGen initCmdArgs(String[] args) {
		PrimeSetsGen primeSetsGen = null;
		boolean error = false;
		try {
			if (args.length == 0) {
				error = true;
			} else if (args.length == 1) {
				primeSetsGen = new PrimeSetsGen(Integer.parseInt(args[0]),
						false, -1);
			} else if (args.length == 2) {
				primeSetsGen = new PrimeSetsGen(Integer.parseInt(args[1]),
						Boolean.parseBoolean(args[0]), -1);
			} else if (args.length == 3) {
				primeSetsGen = new PrimeSetsGen(Integer.parseInt(args[1]),
						Boolean.parseBoolean(args[0]),
						Integer.parseInt(args[2]));
			}

		} catch (Exception e) {
			error = true;
		}
		if (error) {
			System.out
					.println("Usage: java PrimeSetsGen [true/false] n [limit]");
			System.exit(0);
		}
		return primeSetsGen;
	}

	/*
	 * Handles the clean up and graceful exit of the program
	 */
	void endProgram() {
		if (outToFile && fw != null) {
			try {
				fw.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			fw = null;
		}
		System.out.println("***Total Time Taken in milliseconds: "
				+ (System.currentTimeMillis() - startTime) + "ms");
		System.exit(0);
	}

	/************************************************************/
	/* END: UTILITY AND STANDARD METHODS FOR INIT/EXIT/IO */
	/************************************************************/

}
