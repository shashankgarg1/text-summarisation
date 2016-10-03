import java.lang.*;

import org.rosuda.JRI.*;
//import org.rosuda.JRI.Rengine;

import org.rosuda.REngine.REngineException;
import org.rosuda.REngine.JRI.JRIEngine;

public class Rinjava {

 public static void main(String a[]) throws REngineException
 {
  Rengine re = new Rengine (new String[]{"--save"}, false, null);  
  re.eval("source(\"C:/Users/Atul/Desktop/Summ/basic.r\")");
  
  re.eval("source(\"C:/Users/Atul/Desktop/Summ/makesummary.r\")");
  re.eval("source(\"C:/Users/Atul/Desktop/Summ/classifier.R\")");
  
  //take location of file as input and pass it in makesummary
  
  //re.eval("summary1<-makesummary(\"F:/summ dataset/inputdata\",t)");
  //System.out.println(re.eval("summary1"));	//the summary is displayed
 
  re.end();
  
 }
}
