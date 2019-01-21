import java.util.LinkedList;

public class Sieve{
  static boolean[] sieve(int n)
  {
    boolean primes[] = new boolean[n];
    for(int i=0;i<n;i++)
      primes[i] = true;

    for(int i = 2; i*i <= n; i++)
    {
      if(primes[i] == true)
      {
        for(int j = i*2; j <= n; j += i){
          primes[j] = false;
        }
      }
    }
    return primes;
  }
  public static void main(String[] args) {
    int n = Integer.parseInt(args[0]);
    if(n < 0) throw new java.lang.RuntimeException("Input not a positive integer.");
    boolean[] primes = sieve(n);
    for(int i = 2; i < n; i++){
      if(primes[i]) System.out.print(i + " ");
    }
    System.out.println("");
  }
}
