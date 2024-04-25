---
title: Regression with Complex Numbers in Python
---

<p>
This is a small aside that didn't quite fit into my article about linear regression. Suppose that we want to do regression with complex numbers. 
When doing regression with real numbers, we looked at squared residuals, but the complex numbers have a distance metric that is slightly different. How can we accomplish this? Let's first take
a look at visualizing complex-valued functions in Python. 
</p>

<div class='heading'>Complex Functions in Python</div><hr/>

<p>
For simplicity, I will be restricting myself to functions of one complex variable so that we have clear visualizations.
This construction is fairly straightforward, so I'll just give the code and then a brief review of what is happening.
</p>

<p>
<code>
<pre class = "prettyprint">
import cmath
import numpy as np
import pandas as pd
%matplotlib inline
import matplotlib.pylab as plt
from numpy.random import multivariate_normal

class complex_function:
    def __init__(self, grid_min, grid_max, grid_step, function):
        real_grid = np.arange(grid_min, grid_max, grid_step)
        complex_grid = np.arange(grid_min, grid_max, grid_step)
        xx, yy = np.meshgrid(real_grid, complex_grid, sparse=True)
        
        self.input_grid = np.vectorize(complex)(xx, yy).ravel()
        self.output = function(self.input_grid).ravel()
        self.function = function
        
    def plot(self):
        plt.figure(figsize=(20,20))

        for point in range(len(self.input_grid)):
            plt.plot( [self.input_grid.real[point], self.output.real[point]], 
                      [self.input_grid.imag[point], self.output.imag[point]], 
                      color = 'blue', 
                      linewidth=.2,)
        

        
        plt.scatter(self.input_grid.real, 
                    self.input_grid.imag, 
                    color = 'blue',
                    label = 'Input Grid')
        
        plt.scatter(self.output.real, 
                    self.output.imag, 
                    color = 'red',
                    label = 'Output Grid')

        plt.legend(prop={'size': 20})
                
        
    def generate_sample(self, var_scale, plot = False):
        #this is a complex normal distribution for noise
        
        real_noise, imag_noise = zip(*multivariate_normal(mean = (0, 0), 
                                                          cov = var_scale*np.identity(2), 
                                                          size = len(self.input_grid)))
        
        complex_noise = np.vectorize(complex)(np.array(real_noise), 
                                              np.array(imag_noise))
               
        with_noise = self.output + complex_noise
        
        if plot:
        
            self.plot()
            plt.scatter(with_noise.real, 
                        with_noise.imag, 
                        color = 'green',
                        label = 'Generated Sample')
            
            plt.legend(prop={'size': 20})
            
        return with_noise
</pre>
</code>
</p>

<p>
This class can both visualize a complex function of one variable and generate a sample from that given function. The class is initialized by passing a grid min, max, and step size to determine a grid of input values along with a function to be applied. In the plot method, we simply plot the input and output grids, drawing a line for each evaluation of the function.
</p>

</p>
The initialization also stores the input grid as well as the result of applying the function. As an example, suppose that I have the function $z = (1+i)w + (-1+2i)$. I can define this
function:
</p>

<p>
<code>
<pre class = "prettyprint">
def f(w):
    return complex(1,1)*w+complex(-1,2)
</pre>
</code>
</p>

<p>
I will create an instance of my class:
</p>

<p>
<code>
<pre class = "prettyprint">
test = complex_function(grid_min = -2, 
                        grid_max =  2,
                        grid_step= .1, 
                        function =  f)
</pre>
</code>
</p>

<p>
Now if I call the method <code class = "prettyprint">test.plot()</code> I will have the following graph:
</p>

<p>
<img src="../static/article_files/complex_regression/function.png" height="900" width="900">
</p>

<p>
Finally in the <code class = "prettyprint">generate_sample</code> method, we add a normally distributed amount of noise to each evaluation of the function. We can specify 
<code class = "prettyprint">var_scale</code>, a constant that is multiplied by our covariance matrix to determine the
spread of our added noise. I have chosen to make this covariance matrix diagonal, representing that the noise added in the real and complex directions is independent. 
</p>

<p>
Calling this method with <code class = "prettyprint">test.generate_sample(var_scale= .1, plot = True)</code> gives:
</p>

<p>
<img src="../static/article_files/complex_regression/sample.png" height="900" width="900">
</p>

<div class='heading'>Implementing Linear Regression</div><hr/>

<p>
Now that we have a bit of a handle on things, how do we perform our regression? Suppose that we have input $w_j$ and output $z_j$. I will define our loss using the complex modulus, giving the following:
</p>

<p>
$$
\underset{\beta_0, \beta_1}{\operatorname{arg\,min}} \sum_{j=1}^n ||z_j - \left(\beta_0 + \beta_1 w_j\right)||^2 = \underset{\beta_0, \beta_1}{\operatorname{arg\,min}} \sum_{j=1}^n \left(\overline z_j - \left(\overline{\beta_0} + \overline{\beta_1} \overline w_j\right)\right) \left(z_j - \left(\beta_0 + \beta_1 w_j\right)\right).
$$
</p>

<p>
Here I have used the identity that $||z||^2 = z \overline z$ for any complex number. We can see that when we take the derivative and solve, the only difference will be the inclusion of the conjugate transpose in place of just
the transpose. So we have the solution:
</p>

<p>
$$
\hat\beta = \left(X^*X\right)^{-1}X^* z.
$$
</p>

<p>
where $X$ is our matrix of observations $w_j$ and $X^* = \overline{X}^T$ is the "conjugate transpose". (Note that there is nothing particuliar about this solution being for one independent variable).
</p>

<p>
In Python, I can implement this as another class object which is initialized with two arrays of observations, with $W$ as our independent variable and $Z$ as the response variable:
</p>

<p>
<code>
<pre class = "prettyprint">
class complex_linear_regression:
    def fit(self, W, Z):
        W = W.ravel()
        Z = Z.ravel()
        
        X = self.design(W)
        X_star = X.conj().T
        Z = Z.reshape(-1, 1)
        
        self.beta = np.matmul(np.matmul(np.linalg.inv(np.matmul(X_star, X)), X_star), Z)
        
    def predict(self, W, plot = False):
        W = W.ravel()
        X = self.design(W)
        pred = np.matmul(self.beta.T, X.T).ravel()   

        return pred
    
    def residual(self, W, Z, plot = False):
        pred = self.predict(W)

        if plot:
            plt.figure(figsize=(20,20))
            plt.scatter(pred.real, pred.imag, color = 'black', label='Predictions')
            plt.scatter(Z.real, Z.imag, color = 'green', label = 'Actual Values')
            plt.legend(prop={'size': 20})
            
        polar = np.vectorize(cmath.polar)(Z - pred)
        polar = polar[0]
        return np.sum(polar)/len(W)

    def design(self, W):
        W = W.ravel()
        return np.c_[np.ones(len(W)), W.reshape(-1, 1)]
</pre>
</code>
</p>

<p>
As an example, I will use the same function I used above:
</p>

<p>
<code>
<pre class = "prettyprint">
Z = test.generate_sample(var_scale = .1, plot = False)
W = test.input_grid
</pre>
</code>
</p>

<p>
I first define and fit the regression to my data:
</p>

<p>
<code>
<pre class = "prettyprint">
reg = complex_linear_regression()
reg.fit(W, Z)
</pre>
</code>
</p>

<p>
At this point I am able to view the regressions's coefficients <code class = "prettyprint">reg.beta</code>:
</p>

<p>
<code>
<pre>
array([[-0.99465742+2.0105358j ],
       [ 0.99627609+1.00742867j]])
</pre>
</code>
</p>

<p>
As I only added a very small amount of noise, we can see that the predicted coefficients are very close to the actual distribution. 
</p>

<p>
I can use the predict method to see what values my regression will predict and compare the predicted and actual values for our target variable:
</p>

<p>
<code>
<pre class = "prettyprint">
predictions = reg.predict(W, plot = False)
values = {'Actual':Z, 'Predicted':predictions}
residual = pd.DataFrame(values)
</pre>
</code>
</p>

<p>
Here is the data frame (as appears in Jupyter):
</p>

<p>
<img src="../static/article_files/complex_regression/df.png">
</p>

<p>
I have also included a method to visualize these residuals and return the mean loss (the difference in modulus between predicted and actual values):
</p>

<p>
<code>
<pre class = "prettyprint">
reg.residual(W, Z, plot = True)
</pre>
</code>
</p>

<p>
The above shows that the prediction has a mean modulus loss of 0.40703018315821154 and provides the following plot:
</p>

<p>
<img src="../static/article_files/complex_regression/residuals.png" height="900" width="900">
</p>

<p>
Code used in this article can be found <a href="https://github.com/chenson2018/website-code/tree/master/Regression%20with%20Complex%20Numbers%20in%20Python" target="blank">here</a>.
</p>


