import scala.annotation.tailrec
object PascalTriangle {

  def pascalTriangle(k:Int): Unit = {

    @tailrec
    def iter(r:Int,c:Int,N:Int):Unit={
      if(r==(N-1) && c==(N)) return;
      else if(c<=r)
      {
        printf("%d ",comb(r,c).asInstanceOf[Int]);
        iter(r,c+1,N);
      }
      else
      {
        println();
        iter(r+1,0,N);
      }
    }



    def factorial(n:Int):Int ={

      if(n==0)
        return 1;
      @tailrec
      def iter2(n:Int,x:Int):Int = {
        if(n==1) return x;
        else
          iter2(n-1,n*x);
      }
      iter2(n,1);
    }

    def comb(n:Int,r:Int):Double = {
      factorial(n)/(factorial(r)*factorial(n-r));
    }

    iter(0,0,k);    //start iteration here
  }

  def main(args: Array[String]) {
    
    pascalTriangle(readInt());
  }
}