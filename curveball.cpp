#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List curveball_cpp(Rcpp::List inputList, int numSwaps) {

  //get number of rows
  int numRows = inputList.length(); 

  //convert input list into a 2D std::vector
  std::vector<std::vector<int>> oneLocs (numRows);
  for(int i = 0; i < numRows; i++) {
    oneLocs[i] = Rcpp::as<std::vector<int> > (inputList[i]);
  }

  //conduct row swaps a total of (numSwaps) times
  for (int i = 1; i <= numSwaps; i++) {

    //get two random row numbers
    int r1Index = R::runif(0,1) * numRows;
    int r2Index = r1Index;
    while (r2Index == r1Index) {
      r2Index = R::runif(0,1) * numRows;
    }

    //create references to the two rows being mixed
    std::vector<int> & r1 = oneLocs[r1Index];
    std::vector<int> & r2 = oneLocs[r2Index];
    if (r1.size() == 0 || r2.size() == 0) {
      continue;
    }

    //sort rows being mixed
    std::sort(r1.begin(), r1.end());
    std::sort(r2.begin(), r2.end());

    //generate iterators for first pass through rows
    std::vector<int>::iterator first1 = r1.begin();
    std::vector<int>::iterator last1 = r1.end();
    std::vector<int>::iterator  first2 = r2.begin();
    std::vector<int>::iterator  last2 = r2.end();
    int intersectionLength = 0;
    int symDiffLength = 0;
    std::vector<int> intersect (r1.size());
    std::vector<int> sym_diff (r1.size() + r2.size());

    //simultaneously find intersection and symmetric difference of r1 and r2
    //compare elements in r1 and r2 until end of a vector is reached
    while (first1!=last1 && first2!=last2)
    {
      //element in row1 is less than row2, member of symmetric difference
      if (*first1<*first2) {
        //use swapLocations to add element to n1 or n2
        sym_diff[symDiffLength++] = *first1;
        //increment iterators

        ++first1;
      }
      //element in row1 is greater than row2, member of symmetric difference
      else if (*first2<*first1) {
        //use swapLocations to add element to n1 or n2
        sym_diff[symDiffLength++] = *first2;
        //increment iterators
        ++first2;
      }
      //element in row1 is equal to row2, member of intersection
      else {
        //add element to both arrays and increment both iterators
        intersect[intersectionLength++] = *first1;
        ++first1; ++first2;
      }
    }//end while

    //pass through remainder of r1, all members of symmetric difference
    while (first1 != last1) {
      sym_diff[symDiffLength++] = *first1;
      ++first1;
    }

    //pass through remainder of r2, all members of symmetric difference
    while (first2 != last2) {
      sym_diff[symDiffLength++] = *first2;
      ++first2;
    }

    if (symDiffLength == 0) {
      continue;
    }

    intersect.resize(intersectionLength);
    sym_diff.resize(symDiffLength);


    //shuffle the symmetric difference
    for (int i = 0; i < sym_diff.size() - 1; i++) {
      int j = i + R::runif(0,1) * (sym_diff.size() - i);
      std::swap(sym_diff[i],sym_diff[j]);
    }

    //assemble shuffled r1 and r2 from intersection and shuffled symmetric difference
    int split_point = r1.size() - intersect.size();
    r1.clear();
    r2.clear();
    r1.insert(r1.end(), intersect.begin(), intersect.end());
    r1.insert(r1.end(), sym_diff.begin(), sym_diff.begin()  + split_point);
    r2.insert(r2.end(), intersect.begin(), intersect.end());
    r2.insert(r2.end(), sym_diff.begin() + split_point, sym_diff.end());
  }

  //Return randomized adjacency list
  Rcpp::List randomizedList (numRows);

  for (int i = 0; i < numRows; i++) {
    Rcpp::IntegerVector temp = Rcpp::wrap(oneLocs[i]);
    randomizedList[i] = temp;
  }
  return randomizedList;
}

