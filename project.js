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

// Adds matrix m1 to this matrix
Matrix.prototype.add = function(m1) {
    if (this.rows !== m1.rows) {
        throw "Rows don't match up";
    }
    if (this.cols !== m1.cols) {
        throw "Columns don't match up";
    }

    var n = new Matrix(this.rows, this.cols, 0);
    for (r = 0; r < this.rows; r++) {
        for (c = 0; c < this.cols; c++) {
            n.data[r][c] = this.data[r][c] + m1.data[r][c];
        }
    }
    return n;
};

// Subtracts matrix m1 from this matrix
Matrix.prototype.sub = function(m1) {
    if (this.rows !== m1.rows) {
        throw "Rows don't match up";
    }
    if (this.cols !== m1.cols) {
        throw "Columns don't match up";
    }
	
    var n = new Matrix(this.rows, this.cols, 0);
	
    for (r = 0; r < this.rows; r++) {
        for (c = 0; c < this.cols; c++) {
            n.data[r][c] = this.data[r][c] - m1.data[r][c];
        }
    }
    return n;
};

// Computes this * B for B a matrix or scalar
Matrix.prototype.mult = function(B){
    if (B instanceof Matrix) {
        var m = new Matrix(this.rows, B.cols, 0);

        if (this.cols !== B.rows) {
            throw "This matrix columns != matrix 1 rows";
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

        for(i = 0; i < A.cols; i++) {
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
		for (x=0; x<A.length; x++) {
			a += Math.pow(A[x],p);
		}
		return Math.pow(a,(1.0/p));
	}
};