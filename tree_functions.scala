ef treeMap[A, B](tree: Tree[A])(f: A => B): Tree[B] =
		tree match {
			case EmptyTree => EmptyTree
			case Node(x, left, right) => Node(f(x), tree2Map(left)(f), tree2Map(right)(f))
		}                               
	
	def length[A](tree: Tree[A]): Int =
	 tree match {
	 	case EmptyTree => 0
	 	case Node(x, left, right) => length2(left) + 1 + length2(right)
	 }                                    
	 
	 def maxDepth[A](tree: Tree[A]): Int =
	   tree match {
	   	case EmptyTree => 0
	   	case Node(x, left, right) =>
	   		var lmax = maxDepth2(left)
	   		var rmax = maxDepth2(right)
	   		if (lmax > rmax) lmax + 1
	   		 else rmax + 1
	   }                                      
	  
	  def loadTree[A](tree: Tree[A]): A =
	  	tree match {
	  		case EmptyTree => throw new Exception("value of empty tree")
	  		case Node(x, left, right) => x
	  	}                            
	  
	  def leftBranch[T](tree: Tree[T]): Tree[T] =
	   tree match {
	   	case EmptyTree => throw new Exception("left branch of empty tree")
	   	case Node(x, left, right) => left
	   	}                                
	   
	   def rightBranch[T](tree: Tree[T]): Tree[T] =
	   tree match {
	   	case EmptyTree => throw new Exception("left branch of empty tree")
	   	case Node(x, left, right) => right
	   	}                              
	  
	  def isTreeEmpty[T](tree: Tree[T]): Boolean =
	  	tree match {
	  		case EmptyTree => true
	  		case Node(x, left, right) => false
	  	}                               
	 
	 
	 def addToTree(elem: Int, tree: Tree[Int]): Tree[Int] = {
	 	
	 		if (isTreeEmpty(tree)) Node(elem, EmptyTree, EmptyTree)
	 		else if (elem == loadTree(tree)) tree
	 		else if (elem < loadTree(tree)) Node(loadTree(tree), addToTree(elem, leftBranch(tree)), rightBranch(tree))
	 		else Node(loadTree(tree), leftBranch(tree), addToTree(elem, rightBranch(tree)))
	 	
	 	} 
