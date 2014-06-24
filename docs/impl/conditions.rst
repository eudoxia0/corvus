**********
Conditions
**********

Defining Conditions
===================

The :code:`defcondition` form, after defining a condition, creates a global
variable known as the *global condition handler*. This is a pointer to the function
that handles the condition, and is by default set to NULL.

Handling Conditions
===================

The :code:`handling` form is implemented very simply:

1. For each `(condition, handler)` pair, create a temporary variable (Call it
the *previous handler*) to hold the current value of the condition's global
condition handler.

2. Then, set the global condition handler to the value of `handler`.

3. Execute the body of the :code:`handling` form.

4. For each `(condition, handler)` pair, restore the value of the global
condition handler for that condition to the value held in the temporary

Essentially, the :code:`handling` form is a stack. However, it keeps the stack
of handlers in the call stack, rather than the heap. This allows us to have
conditions without depending on heap allocation, which might not be available.
This way, code running in devices without memory may use conditions at no
cost.
