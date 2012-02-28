// Created by Kimberly Barchenger and Jennifer Shaw
// This code is distributed under the BSD license and it is a rewrite of code
// shared in class CSC431 at DePaul University by Massimo Di Pierro.

Matrix = function(rows, cols, fill) {
    var data = [];
    var a;
	
    if (cols < 1 || rows < 1) {
        throw "Rows and columns must each be at least 1";
    }

    // Initialize matrix
    if (fill instanceof Array) {
        var counter = 0;
        for (i = 0; i < rows; i++) {
            a = [];
            for (j=0; j < cols; j++) {
                a[j] = fill[counter];
                counter++;
            }
            data[i] = a;
        }
    }
    else if (typeof(fill) === 'number') {
        for (i = 0; i < rows; i++) {
            a = [];
            for (j = 0; j < cols; j++) {
                a[j] = fill;
            }
            data[i] = a;
        }
    }
    else {
        alert("Fill must be a number or an array");
    }

    this.rows = rows;
    this.cols = cols;
    this.data = data;
};

print_matrix = function(m) {
    console.log('[');
    for (i = 0; i < m.rows; i++) {
        console.log('  [' + m.data[i] + ']');
    }
    console.log(']');
};

// Creates an identity matrix of size rows
identity_matrix = function(rows) {
    var m = new Matrix(rows, rows, 0);
    for (i = 0; i < rows; i++) {
        m.data[i][i] = 1;
    }
    return m;
};

// Creates a matrix with the diagonal filled from the list d
diagonal_matrix = function(d) {
    m = new Matrix(d.length, d.length, 0);

    for (i = 0; i < d.length; i++) {
        m.data[i][i] = d[i];
    }

    return m;
};

// Builds a matrix from a list of lists
matrix_from_list = function(l) {
    var a = [];
    for (i = 0; i < l.length; i++) {
        a = a.concat(l[i]);
    }
    return new Matrix(l.length, l[0].length, a);
};

// Negates this matrix
Matrix.prototype.neg = function() {
    var n = new Matrix(this.rows, this.cols, 0);
    for (i = 0; i < this.rows; i++) {
        for (j = 0; j < this.cols; j++) {
            n.data[i][j] = -this.data[i][j];
        }
    }
    return n;
}

// Computes this + B for B a matrix or scalar
Matrix.prototype.add = function(B) {
    if (B instanceof Matrix) {
        if (this.rows !== B.rows) {
            throw "Rows don't match up";
        }
        if (this.cols !== B.cols) {
            throw "Columns don't match up";
        }

        var m = new Matrix(this.rows, this.cols, 0);
        for (r = 0; r < this.rows; r++) {
            for (c = 0; c < this.cols; c++) {
                m.data[r][c] = this.data[r][c] + B.data[r][c];
            }
        }
        return m;
    }
    else if (typeof(B) === 'number') {
        var m = new Matrix(this.rows, this.cols, 0);
        for (i = 0; i < m.rows; i++) {
            for (j = 0; j < m.cols; j++) {
                m.data[i][j] = this.data[i][j] + B;
            }
        }
        return m;
    }
    else {
        throw "B must be a matrix or number.";
    }
};

// Computes this - B for B a matrix or scalar
Matrix.prototype.sub = function(B) {
    if (B instanceof Matrix) {
        if (this.rows !== B.rows) {
            throw "Rows don't match up";
        }
        if (this.cols !== B.cols) {
            throw "Columns don't match up";
        }

        var m = new Matrix(this.rows, this.cols, 0);

        for (r = 0; r < this.rows; r++) {
            for (c = 0; c < this.cols; c++) {
                m.data[r][c] = this.data[r][c] - B.data[r][c];
            }
        }
        return m;
    }
    else if (typeof(B) === 'number') {
        var m = new Matrix(this.rows, this.cols, 0);
        for (i = 0; i < m.rows; i++) {
            for (j = 0; j < m.cols; j++) {
                m.data[i][j] = this.data[i][j] - B;
            }
        }
        return m;
    }
    else {
        throw "B must be a matrix or number.";
    }
};

// Computes this * B for B a matrix or scalar
// If this and B both single column matrices with same number of rows,
// compute scalar product
Matrix.prototype.mult = function(B){
    if (B instanceof Matrix) {
        var m = new Matrix(this.rows, B.cols, 0);

        if (this.cols !== B.rows) {
            // Return scalar product
            if (this.cols === 1 && B.cols === 1 && this.rows === B.rows) {
                // Return scalar product
                var sum = 0;
                for (r = 0; r < this.rows; r++) {
                    sum += this.data[r][0] * B.data[r][0];
                }
                return sum;
            }
            else {
                throw "This matrix columns != matrix 1 rows";
            }
        }
	
        for (r = 0; r < this.rows; r++) {
            for (c = 0; c < B.cols; c++) {
                var k = 0;
                for (n = 0; n < this.cols; n++) {
                    k += this.data[r][n]*B.data[n][c];
                }
                m.data[r][c] = k;
            }
        }
        return m;
    }
    else if (typeof(B) === 'number') {
        var m = new Matrix(this.rows, this.cols, 0);
        for (i = 0; i < m.rows; i++) {
            for (j = 0; j < m.cols; j++) {
                m.data[i][j] = B*this.data[i][j];
            }
        }
        return m;
    }
    else {
        throw "B must be a matrix or number.";
    }
};

// Computes this / B for B a matrix or scalar
Matrix.prototype.div = function(B) {
    var m = new Matrix(this.rows, this.cols, 0);

    if (B instanceof Matrix) {
        // Matrix/Matrix
        var n = B.inverse();
        m = this.mult(n);
    }
    else if (typeof(B) === 'number') {
        // Matrix/Scalar
        m = this.mult(1.0/B);
    } 
    else {
        throw "B must be a matrix or number.";
    }

    return m;
};

// Computes the inverse of this matrix using Gauss-Jordan
Matrix.prototype.inverse = function(){
    if (this.rows != this.cols) {
        throw "Not a square matrix";
    }

    var m, p, q;
    var A = matrix_from_list(this.data);
    var B = identity_matrix(A.rows);

    for (c = 0; c < A.cols; c++) {
        var r = c + 1;
        for(r; r < 2; r++) {
            if(Math.abs(A.data[r][c]) > Math.abs(A.data[c][c])) {
                A = A.swap_rows(r,c);
                B = B.swap_rows(r,c);			
            }
        }

        p = A.data[c][c];

        for (i = 0; i < A.cols; i++) {
            A.data[c][i] = A.data[c][i] / p;
            B.data[c][i] = B.data[c][i] / p;
        }			

        for (r = 0; r < A.rows; r++){
            if (r !== c) {
                q = A.data[r][c];
                for(i = 0; i < A.cols; i++) {
                    A.data[r][i] = A.data[r][i] - q*A.data[c][i];
                    B.data[r][i] = B.data[r][i] - q*B.data[c][i];
                }
            }
        }
    }

    return B;
};

// Swap row i with row j in this matrix
Matrix.prototype.swap_rows = function(i, j) {
    var m = new Matrix(this.rows, this.cols, 0);
	
    for (r = 0; r < m.rows; r++) {
        if (r === i) {
            m.data[r] = this.data[j];
        }
        else if (r === j) {
            m.data[r] = this.data[i];
        }
        else {
            m.data[r] = this.data[r];
        }
    }

    return m;
};

// Transpose this matrix
Matrix.prototype.transpose = function() {
    var m = new Matrix(this.cols, this.rows,0);

    for (c = 0; c < this.cols; c++) {
        for (r = 0; r < this.rows; r++) {
            m.data[c][r] = this.data[r][c];
        }
    }

    return m;
}

// Returns true if this matrix is almost symmetric
Matrix.prototype.is_almost_symmetric = function() {
    if (this.rows !== this.cols) { return false; }
	
    var ap = 1e-6;
    var rp = 1e-4;
	
    for (r = 0; r < this.rows; r++) {
        for (c = 0; c < this.rows; c++) {
            var delta = Math.abs(this.data[r][c] - this.data[c][r])
            if (delta > ap && delta > Math.max(Math.abs(this.data[r][c]), 
                Math.abs(this.data[c][r]))*rp) { 
                return false;
            }
        }
    }
    return true;
}

// Returns true if this matrix is almost zero
Matrix.prototype.is_almost_zero = function() {
    var ap = 1e-6;
    var rp = 1e-4;

    for (r = 0; r < this.rows; r++) {
        for (c = 0; c < this.cols; c++) {
            var delta = Math.abs(this.data[r][c] - this.data[c][r])
            if (delta > ap && delta > Math.max(Math.abs(this.data[r][c]), 
                Math.abs(this.data[c][r]))*rp) { 
                return false;
            }
        }
    }
    return true;
}

//Norm of an array
normA = function(A,p){
    if (A instanceof Array) {
        var a = 0;
        for (x = 0; x < A.length; x++) {
            a += Math.pow(A[x],p);
        }
        return Math.pow(a,(1.0/p));
    }
};

//Norm of a matrix
normM = function(A,p){
	if (A instanceof Matrix) {
		var x=[];
		var y=0;
		var z=[];
		for (r=0;r<A.rows;r++){
			for (c=0;c<A.cols;c++){
				x.push(Math.pow(A.data[r][c],p));
			}
			y += normA(x,p);
			z.push(y);
		}
		if (A.rows===1 || A.cols===1){
			return Math.pow(y,(1.0/p));
		}
		else if (p===1){
			return Math.max.apply(Math, z);;
		}
		else{
			throw "Not implemented error";
		}
	}
	else {
		return Math.abs(A);
	}
};

//Condition number
condition_number = function(f){
	if (f instanceof Matrix) {
		var a = f.inverse();
		return normM(f,1)*normM(a,1);
	}
	else{
		throw "Not implemented error";
	}
};

//Exp of a number or matrix
exp = function(x, ap, rp, ns){
	if (x instanceof Matrix){
		var t = Matrix.identity(x.cols);
		var s = t;
		var i=0;
		for (k=1; k< ns; k++){
			i = x.div(k);
			t = i.mult(t);
			s = t.add(s)
			if(normM(t,1)<Math.max(ap,normM(s,1)*rp)){
				return s;
			}
		}
		throw "Arithmetic Error - no convergence";
	}
	else{
		return Math.exp(x);			
	}
};


// Runs the Cholesky decomposition on the array A
Cholesky = function(A) {
    if (!A.is_almost_symmetric()) {
        throw "Is not symmetric";
    }
	
    L = matrix_from_list(A.data);
	
    for (k = 0; k < L.cols; k++) {
        if (L.data[k][k] <= 0) {
            throw "Not positive definite";
        }
        p = Math.sqrt(L.data[k][k]);
        L.data[k][k] = Math.sqrt(L.data[k][k]);
        for (i = k+1; i < L.rows; i++) {
            L.data[i][k] /= p;
        }
        for (j = k+1; j < L.rows; j++) {
            p = L.data[j][k];
            for (i = k+1; i < L.rows; i++) {
                L.data[i][j] -= p*L.data[i][k];
            }
        }
    }
    for (i = 0; i < L.rows; i++) {
        for (j = i+1; j < L.cols; j++) {
            L.data[i][j] = 0;
        }
    }
	
    return L;
}

// Returns true if the matrix A is positive definite
// Uses Cholesky(A) to determine
is_positive_definite = function(A) {
    if (!A.is_almost_symmetric()) {
        return false;
    }

    try {
        Cholesky(A);
        return true;
    }
    catch (err) {
        return false;
    }
}

// Assesses Markovitz risk/return and returns portfolio, return, and risk
// as an array
// Takes matrix mu, matrix A, and number risk free return
Markovitz = function(mu, A, r_free) {
    var x, y, temp, A_inv, portfolio_risk, portfolio_return;

    x = new Matrix(A.rows, 1, 0.0);
    temp = mu.sub(r_free);
    A_inv = A.inverse()
    x = A_inv.mult(temp);

    y = 0;
    for (r = 0; r < x.rows; r++) {
        y += parseFloat(x.data[r]);
    }

    x = x.div(y);
    portfolio = new Matrix(1, x.rows, 0.0);

    for (r = 0; r < x.rows; r++) {
        portfolio.data[0][r] = x.data[r];
    }

    portfolio_return = mu.mult(x);
    temp = A.mult(x)
    portfolio_risk = Math.sqrt(x.mult(temp))
    return [portfolio, portfolio_return, portfolio_risk]
}
	
//Solve Fixed Point 
solve_fixed_point = function(f,x,ap,rp,ns){
	g(x) = function(){
		return f(x) + x;
	}
	Dg = D(g);
	for (k=0; k<ns; k++){
		if (Math.abs(Dg(x))>=1){
			throw "error D(g)(x)>=1";
		}
		(x_old,x) = (x, g(x));
		if (k>2 && normM(x_old.sub(x),1)<Math.max(ap,normM(x,1)*rp)){
			return x;
		}
	throw "no convergence";
	}
};