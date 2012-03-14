// Created by Kimberly Barchenger and Jennifer Shaw
// This code is distributed under the BSD license and it is a rewrite of code
// shared in class CSC431 at DePaul University by Massimo Di Pierro.

Matrix = function(rows, cols, fill) {
    // Default values
    fill = fill || 0.0;

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
            for (j = 0; j < cols; j++) {
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
    var n = new Matrix(this.rows, this.cols);
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

        var m = new Matrix(this.rows, this.cols);
        for (r = 0; r < this.rows; r++) {
            for (c = 0; c < this.cols; c++) {
                m.data[r][c] = this.data[r][c] + B.data[r][c];
            }
        }
        return m;
    }
    else if (typeof(B) === 'number') {
        var m = new Matrix(this.rows, this.cols);
        for (r = 0; r < m.rows; r++) {
            for (c = 0; c < m.cols; c++) {
                m.data[r][c] = this.data[r][c] + B;
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

        var m = new Matrix(this.rows, this.cols);

        for (r = 0; r < this.rows; r++) {
            for (c = 0; c < this.cols; c++) {
                m.data[r][c] = this.data[r][c] - B.data[r][c];
            }
        }
        return m;
    }
    else if (typeof(B) === 'number') {
        var m = new Matrix(this.rows, this.cols);
        for (r = 0; r < m.rows; r++) {
            for (c = 0; c < m.cols; c++) {
                m.data[r][c] = this.data[r][c] - B;
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
        var m = new Matrix(this.rows, B.cols);

        if (this.cols !== B.rows) {
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
        var m = new Matrix(this.rows, this.cols);
        for (r = 0; r < m.rows; r++) {
            for (c = 0; c < m.cols; c++) {
                m.data[r][c] = B*this.data[r][c];
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
    var m = new Matrix(this.rows, this.cols);

    if (B instanceof Matrix) {
        // Matrix/Matrix
        var n = B.inverse();
        m = this.mult(n);
    }
    else if (typeof(B) === 'number') {
        // Matrix/Scalar
        m = this.mult(1.0 / B);
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
    var m = new Matrix(this.rows, this.cols);
	
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
    var m = new Matrix(this.cols, this.rows);

    for (c = 0; c < this.cols; c++) {
        for (r = 0; r < this.rows; r++) {
            m.data[c][r] = this.data[r][c];
        }
    }

    return m;
}

// Returns true if this matrix is almost symmetric
Matrix.prototype.is_almost_symmetric = function(ap, rp) {
    if (this.rows !== this.cols) { return false; }
	
    // Default values
    ap = ap || 1e-6;
    rp = rp || 1e-4;
	
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
Matrix.prototype.is_almost_zero = function(ap, rp) {
    // Default values
    ap = ap || 1e-6;
    rp = rp || 1e-4;

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

// Finds the p-norm of a matrix or array
norm = function(A, p) {
    //Default p value
    p = p || 1;
    if (A instanceof Array) {
        var a = 0;
        for (x = 0; x < A.length; x++) {
            a += Math.pow(A[x],p);
        }
        return Math.pow(a,(1.0/p));
    }
    else if (A instanceof Matrix) {
        var x = [];
        var y = 0;
        var z = [];
        if (A.rows === 1 || A.cols === 1){
            for (r = 0; r < A.rows; r++) {
                for (c = 0; c < A.cols; c++) {
                    x.push(Math.pow(A.data[r][c], p));
                }
            }
            y += norm(x,p);
            return Math.pow(y, (1.0/p));
        }
        else if (p === 1) {
            for (c = 0; c < A.cols; c++){
                var sumCol = 0;
                for (r = 0; r < A.rows; r++){
                    sumCol += Math.abs(A.data[r][c]);
                }
                z.push(sumCol);
            }
            return Math.max.apply(Math, z);;
        }
        else {
            throw "Not implemented error";
        }
    }
    else {
        return Math.abs(A);
    }
};

// Condition number
condition_number = function(f,x,h) {
	x = x || "None";
	h = h || 1e-6;

	if (typeof(f) === "function"){
		return D(f,x,h)*x/f(x)
	}
    if (f instanceof Matrix) {
        var a = f.inverse();
	 	return norm(f) * norm(a);
    }
    else {
        throw "Not implemented error";
    }
};

// Computes the exp of a number or matrix x
exp = function(x, ns, ap, rp){
    // Default values
    ap = ap || 1e-6;
    rp = rp || 1e-4;

    if (x instanceof Matrix) {
        var t = identity_matrix(x.cols);
        var s = t;
        var i = 0;
        for (k = 1; k < ns; k++) {
            i = x.div(k);
            t = i.mult(t);
            s = t.add(s)
            if(norm(t) < Math.max(ap, norm(s)*rp)){
                return s;
            }
        }
        throw "Arithmetic Error - no convergence";
    }
    else {
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

    x = new Matrix(A.rows, 1);
    temp = mu.sub(r_free);
    A_inv = A.inverse()
    x = A_inv.mult(temp);

    y = 0;
    for (r = 0; r < x.rows; r++) {
        y += parseFloat(x.data[r]);
    }

    x = x.div(y);
    portfolio = new Matrix(1, x.rows);

    for (r = 0; r < x.rows; r++) {
        portfolio.data[0][r] = x.data[r];
    }

    portfolio_return = mu.mult(x);
    temp = A.mult(x);
    portfolio_risk = Math.sqrt(x.mult(temp));
    return [portfolio, portfolio_return, portfolio_risk];
}

// Finds fitting function using least squares method
// Takes a list of points (x, y, dy) and a list of fitting functions
// Returns an object holding:
//    fit_coeff: a column vector with the fitting coefficients
//    chi2: the chi-squared value for the fit
//    fitting_f: the fitting function, as a function of x
fit_least_squares = function(points, f) {
    eval_fitting_function = function(f, c, x) {
        if (f.length === 1) {
            return c.data * f[0](x);
        }
        else {
            var s = 0.0;
            for (i = 0; i < f.length; i++) {
               s += f[i](x)*c.data[i][0];
            }
            return s;
        }
    };

    var A = new Matrix(points.length, f.length);
    var b = new Matrix(points.length, 1);

    for (r = 0; r < A.rows; r++) {
        var weight;
        if (points[r].length > 2) {
            weight = 1.0 / points[r][2];
        }
        else {
            weight = 1.0;
        }
        b.data[r][0] = weight * parseFloat(points[r][1]);
        for (c = 0; c < A.cols; c++) {
            A.data[r][c] = weight * f[c](parseFloat(points[r][0]));
        }
    }

    var At = A.transpose();
    var c = ((At.mult(A)).inverse()).mult(At.mult(b));
    var chi = (A.mult(c)).sub(b);
    var chi2 = Math.pow((norm(chi, 2)), 2);

    fitting_f = function(x) {
        return eval_fitting_function(f, c, x);
    };

    return {
        fit_coeff: c.data,
        chi2: chi2,
        fitting_f: fitting_f
    };
}

// Returns the first derivative of function f at x
D = function(f, x, h){
    // Default value
    h = h || 1e-6;
    
    return (f(x+h) - f(x-h)) / 2 / h;
}


// Returns the second derivative of function f at x
DD = function(f, x, h){
    // Default value
    h = h || 1e-6;

    return (f(x+h) - 2.0*f(x) + f(x-h)) / (h*h);
}

// Finds the zero of function f close to x using the fixed point method
solve_fixed_point = function(f, x, ns, ap, rp){
    // Default values
    ap = ap || 1e-6;
    rp = rp || 1e-4;

    for (k = 0; k < ns; k++){
        if (Math.abs(D(g, x)) >= 1) {
            throw "error D(g)(x) >= 1";
        }
        var x_old = x;
        var x = g(x);
        if (k > 2 && norm(x_old-x) < Math.max(ap, norm(x)*rp)) {
            return x;
        }
    }
    throw "no convergence";
};


// Finds the zero of function f between a and b using the bisection method
solve_bisection = function(f, a, b, ns, ap, rp){
    // Default values
    ap = ap || 1e-6;
    rp = rp || 1e-4;

    var fa = f(a);
    var fb = f(b);
    if (fa === 0) {
        return a;
    }
    if (fb === 0) {
        return b;
    }
    if (fa*fb > 0) {
        throw "f(a) and f(b) must have opposite sign";
    }
    for (k = 0; k < ns; k++) {
        x = (a+b)/2;
        fx = f(x);
        if (fx === 0 || norm(b-a) < Math.max(ap, norm(x)*rp)){
            return x;
        }
        else if (fx*fa < 0){
            b = x;
            fb = fx;
        }
        else {
            a = x;
            fa = fx;
        }
    }
    throw "no convergence";
}


// Finds the zero of function f near x using the Newton method
solve_newton = function(f, x, ns, ap, rp) {
    // Default values
    ap = ap || 1e-6;
    rp = rp || 1e-4;

    for (k = 0; k < ns; k++) {
        fx = f(x);
        Dfx = D(f, x);
        if (norm(Dfx) < ap) {
            throw "unstable solution";
        }
        var x_old = x;
        var x = g(x);
        if (k > 2 && norm(x_old-x) < Math.max(ap, norm(x)*rp)) {
            return x;
        }
    }
    throw "no convergence";
}

// Finds the zero of function f near x using the secant method
solve_secant = function(f, x, ns, ap, rp) {
    // Default values
    ap = ap || 1e-6;
    rp = rp || 1e-4;

    fx = f(x);
    Dfx = D(f,x);
    for (k = 0; k < ns; k++) {
        if (norm(Dfx) < ap) {
            throw "unstable solution";
        }
        x_old = x;
        fx_old = fx;
        x = x - fx/Dfx;
        if (k > 2 && norm(x_old-x) < Math.max(ap, norm(x)*rp)) {
            return x;
        }
        fx = f(x);
        Dfx = (fx-fx_old) / (x-x_old)
    }
    throw "no convergence"
}

// Finds the zero of function f near x using the Newton stabilized method
solve_newton_stabilized = function(f, a, b, ns, ap, rp) {
    // Default values
    ap = ap || 1e-6;
    rp = rp || 1e-4;

    fa = f(a);
    fb = f(b);
    if (fa === 0){
        return a;
    }
    if (fb === 0){
        return b;
    }
    if (fa*fb > 0) {
        throw "f(a) and f(b) must have opposite sign";
    }
    x = (a+b)/2;
    fx = f(x);
    Dfx = D(f,x);
    for (k = 0; k < ns; k++) {
        x_old = x;
        fx_old = fx;
        if (norm(Dfx) > ap) {
            x = x - fx/Dfx;
        }
        if (x === x_old || x < a || x > b) {
            x = (a+b)/2;
        }
        fx = f(x);
        if (fx === 0 || norm(x-x_old) < Math.max(ap, norm(x)*rp)) {
            return x;
        }
        Dfx = (fx-fx_old) / (x-x_old)
        if (fx*fa < 0) {
            b = x;
            fb = fx;
        }
        else {
            a = x;
            fa = fx;
        }
    }
    throw "no convergence";
}


// Finds the max/min of f between a and b using the bisection method
optimize_bisection = function(f, a, b, ns, ap, rp) {
    // Default values
    ap = ap || 1e-6;
    rp = rp || 1e-4;

    Dfa = D(f,a);
    Dfb = D(f,b);
    if (Dfa === 0) {
        return a;
    }
    if (Dfb === 0) {
        return b;
    }
    if (Dfa*Dfb > 0) {
        throw "D(f)(a) and D(f)(b) must have opposite sign";
    }
    for (k = 0; k < ns; k++) {
        x = (a+b)/2;
        Dfx = D(f,x);
        if (Dfx === 0 || norm(b-a) < Math.max(ap, norm(x)*rp)) {
            return x;
        }
        else if (Dfx * Dfa < 0) {
            b = x;
            Dfb = Dfx;
        }
        else {
            a = x;
            Dfa = Dfx;
        }
    }
    throw 'no convergence';
}


// Finds the max/min of f near x using the Newton method
optimize_newton = function(f, x, ns, ap, rp) {
    // Default values
    ap = ap || 1e-6;
    rp = rp || 1e-4;

    for (k = 0; k < ns; k++) {
        Dfx = D(f,x);
        DDfx = DD(f,x);
        if (Dfx === 0) {
            return x;
        }
        if (norm(DDfx) < ap) {
            throw "unstable solution";
        }
		x_old = x;
		x = x - Dfx/DDfx;
        if (norm(x-x_old) < Math.max(ap, norm(x)*rp)) {
            return x;
        }
    }
    throw "no convergence";
}

// Finds the max/min of f near x using the secant method
optimize_secant = function(f, x, ns, ap, rp) {
    // Default values
    ap = ap || 1e-6;
    rp = rp || 1e-4;

    fx = f(x);
    Dfx = D(f,x);
    DDfx = DD(f,x);
    for (k = 0; k < ns; k++) {
        if (Dfx === 0) {
            return x;
        }
        if (norm(DDfx) < ap) {
            throw "unstable solution";
        }

        x_old = x;
        Dfx_old = Dfx;
        x = x - Dfx/DDfx;
        if (norm(x-x_old) < Math.max(ap, norm(x)*rp)) {
            return x;
        }
        fx = f(x);
        Dfx = D(f,x);
        DDfx = (Dfx - Dfx_old) / (x - x_old);
    }
    throw "no convergence";
}

// Finds the max/min of f between a and b using the Newton stabilized method
optimize_newton_stabilized = function(f, a, b, ns, ap, rp) {
    // Default values
    ap = ap || 1e-6;
    rp = rp || 1e-4;
	
	Dfa = D(f,a);
	Dfb = D(f,b);

    if (Dfa === 0) {
        return a;
    }
    if (Dfb === 0) {
        return b;
    }
    if (Dfa*Dfb > 0) {
        throw "D(f)(a) and D(f)(b) must have opposite sign";
    }

    x = (a+b)/2;
    fx = f(x);
    Dfx = D(f,x);
    DDfx = DD(f,x);
    for (k = 0; k < ns; k++) {
        if (Dfx === 0) {
            return x;
        }
        x_old = x;
        fx_old = fx;
        Dfx_old = Dfx;
		if(norm(DDfx) > ap) {
			x = x - Dfx/DDfx;
		}
		if (x === x_old || x < a || x > b){
			x = (a+b)/2;
		}
        if (norm(x-x_old) < Math.max(ap, norm(x)*rp)) {
            return x;
        }
        fx = f(x)
        Dfx = (fx-fx_old) / (x-x_old)
        DDfx = (Dfx-Dfx_old) / (x-x_old)
        if (Dfx * Dfa < 0) {
            b = x;
            Dfb = Dfx;
        }
        else {
            a = x;
            Dfa = Dfx;
        }
    }
    throw "no convergence";
}

// Finds the max/min of f between a and b using the Golden Search method
optimize_golden_search = function(f, a, b, ns, ap, rp) {
    // Default values
    ap = ap || 1e-6;
    rp = rp || 1e-4;

    tau = (Math.sqrt(5) - 1)/2;
    x1 = a + (1-tau)*(b-a);
    x2 = a + tau*(b-a);
    fa = f(a);
    f1 = f(x1);
    fb = f(b);
    f2 = f(x2);
    for (k = 0; k < ns; k++) {
        if (f1 > f2) {
            a = x1;
            fa = f1;
            x1 = x2;
            f1 = f2;
            x2 = a + tau*(b-a);
            f2 = f(x2);
        }
        else {
            b = x2;
            fb = f2;
            x2 = x1;
            f2 = f1;
            x1 = a + (1-tau)*(b-a);
            f1 = f(x1);
        }
        if (k > 2 && norm(b-a) < Math.max(ap, norm(b)*rp)) {
            return b;
        }
    }
    throw "no convergence";
}
