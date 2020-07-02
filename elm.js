(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.di.dX === region.dE.dX)
	{
		return 'on line ' + region.di.dX;
	}
	return 'on lines ' + region.di.dX + ' through ' + region.dE.dX;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.eZ,
		impl.fj,
		impl.fh,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		e0: func(record.e0),
		fg: record.fg,
		e7: record.e7
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.e0;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.fg;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.e7) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.eZ,
		impl.fj,
		impl.fh,
		function(sendToApp, initialModel) {
			var view = impl.fk;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.eZ,
		impl.fj,
		impl.fh,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.dh && impl.dh(sendToApp)
			var view = impl.fk;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.S);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.P) && (_VirtualDom_doc.title = title = doc.P);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.e3;
	var onUrlRequest = impl.e4;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		dh: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.d8 === next.d8
							&& curr.dJ === next.dJ
							&& curr.d3.a === next.d3.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		eZ: function(flags)
		{
			return A3(impl.eZ, flags, _Browser_getUrl(), key);
		},
		fk: impl.fk,
		fj: impl.fj,
		fh: impl.fh
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { eU: 'hidden', eH: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { eU: 'mozHidden', eH: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { eU: 'msHidden', eH: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { eU: 'webkitHidden', eH: 'webkitvisibilitychange' }
		: { eU: 'hidden', eH: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		ei: _Browser_getScene(),
		fl: {
			eA: _Browser_window.pageXOffset,
			eB: _Browser_window.pageYOffset,
			cT: _Browser_doc.documentElement.clientWidth,
			at: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		cT: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		at: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			ei: {
				cT: node.scrollWidth,
				at: node.scrollHeight
			},
			fl: {
				eA: node.scrollLeft,
				eB: node.scrollTop,
				cT: node.clientWidth,
				at: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			ei: _Browser_getScene(),
			fl: {
				eA: x,
				eB: y,
				cT: _Browser_doc.documentElement.clientWidth,
				at: _Browser_doc.documentElement.clientHeight
			},
			eN: {
				eA: x + rect.left,
				eB: y + rect.top,
				cT: rect.width,
				at: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.eO.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.eO.b, xhr)); });
		$elm$core$Maybe$isJust(request.ex) && _Http_track(router, xhr, request.ex.a);

		try {
			xhr.open(request.e1, request.v, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.v));
		}

		_Http_configureRequest(xhr, request);

		request.S.a && xhr.setRequestHeader('Content-Type', request.S.a);
		xhr.send(request.S.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.eS; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.fi.a || 0;
	xhr.responseType = request.eO.d;
	xhr.withCredentials = request.eE;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? $elm$http$Http$GoodStatus_ : $elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		v: xhr.responseURL,
		fe: xhr.status,
		ff: xhr.statusText,
		eS: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return $elm$core$Dict$empty;
	}

	var headers = $elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3($elm$core$Dict$update, key, function(oldValue) {
				return $elm$core$Maybe$Just($elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Sending({
			fc: event.loaded,
			cD: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			e8: event.loaded,
			cD: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
		}))));
	});
}

function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return $elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return $elm$core$Maybe$Nothing;
	}
}


var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});


// CREATE

var _Regex_never = /.^/;

var _Regex_fromStringWith = F2(function(options, string)
{
	var flags = 'g';
	if (options.dZ) { flags += 'm'; }
	if (options.du) { flags += 'i'; }

	try
	{
		return $elm$core$Maybe$Just(new RegExp(string, flags));
	}
	catch(error)
	{
		return $elm$core$Maybe$Nothing;
	}
});


// USE

var _Regex_contains = F2(function(re, string)
{
	return string.match(re) !== null;
});


var _Regex_findAtMost = F3(function(n, re, str)
{
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex == re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		out.push(A4($elm$regex$Regex$Match, result[0], result.index, number, _List_fromArray(subs)));
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _List_fromArray(out);
});


var _Regex_replaceAtMost = F4(function(n, re, replacer, string)
{
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		return replacer(A4($elm$regex$Regex$Match, match, arguments[arguments.length - 2], count, _List_fromArray(submatches)));
	}
	return string.replace(re, jsReplacer);
});

var _Regex_splitAtMost = F3(function(n, re, str)
{
	var string = str;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		var result = re.exec(string);
		if (!result) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _List_fromArray(out);
});

var _Regex_infinity = Infinity;
var $author$project$Main$LinkClicked = function (a) {
	return {$: 3, a: a};
};
var $author$project$Main$UrlChanged = function (a) {
	return {$: 4, a: a};
};
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.m) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.n),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.n);
		} else {
			var treeLen = builder.m * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.r) : builder.r;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.m);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.n) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.n);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{r: nodeList, m: (len / $elm$core$Array$branchFactor) | 0, n: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {dI: fragment, dJ: host, d1: path, d3: port_, d8: protocol, d9: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$application = _Browser_application;
var $author$project$Main$JsonContent = F3(
	function (title, date, url) {
		return {U: date, P: title, v: url};
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$map3 = _Json_map3;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Main$decodeOfJsonContent = A4(
	$elm$json$Json$Decode$map3,
	$author$project$Main$JsonContent,
	A2($elm$json$Json$Decode$field, 'title', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'date', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'url', $elm$json$Json$Decode$string));
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$html$Html$p = _VirtualDom_node('p');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Main$aboutme = {
	S: _List_fromArray(
		[
			A2(
			$elm$html$Html$h3,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('LMDEXPR a.k.a. Yuki Tajiri')
				])),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('I work as server-side engineer @'),
					A2(
					$elm$html$Html$a,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$href('https://corp.chatwork.com/ja/')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Chatwork Co., Ltd.')
						])),
					$elm$html$Html$text(' (2020/06/01 ~)')
				])),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Droped out Master of Mathematics @'),
					A2(
					$elm$html$Html$a,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$href('https://www.kobe-u.ac.jp/')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Kobe University')
						])),
					$elm$html$Html$text(' (2019/04 ~ 2020/10)')
				])),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Graduated Bachelor of Mathematics @'),
					A2(
					$elm$html$Html$a,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$href('https://www.kobe-u.ac.jp/')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('Kobe University')
						])),
					$elm$html$Html$text(' (2017/04 ~ 2019/03)')
				])),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('Graduated Associate Degree of Engineering @'),
					A2(
					$elm$html$Html$a,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$href('https://kumamoto-nct.ac.jp/')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('NIT, Kumamoto College')
						])),
					$elm$html$Html$text(' (2017/04 ~ 2019/03)')
				]))
		]),
	U: $elm$core$String$fromChar('\u03BB'),
	P: 'About me',
	v: 'about'
};
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$html$Html$i = _VirtualDom_node('i');
var $author$project$Main$contact = {
	S: _List_fromArray(
		[
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$i,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('far fa-envelope')
						]),
					_List_Nil),
					$elm$html$Html$text(' tajiri@chatwork.com')
				])),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$a,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$href('https://twitter.com/lmdexpr')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$i,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('fab fa-twitter')
								]),
							_List_Nil),
							$elm$html$Html$text(' Twitter')
						]))
				])),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$a,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$href('https://github.com/lmdexpr')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$i,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('fab fa-github')
								]),
							_List_Nil),
							$elm$html$Html$text(' Github')
						]))
				])),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$a,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$href('https://note.com/lmdexpr')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$i,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('far fa-sticky-note')
								]),
							_List_Nil),
							$elm$html$Html$text(' Note')
						]))
				])),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$a,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$href('https://www.amazon.jp/hz/wishlist/ls/2Z7ZETUODB09J?ref_=wl_share')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$i,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('fab fa-amazon')
								]),
							_List_Nil),
							$elm$html$Html$text(' Amazon')
						]))
				])),
			A2(
			$elm$html$Html$p,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$a,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$href('https://lmdexpr.github.io')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$i,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('fas fa-angle-double-left')
								]),
							_List_Nil),
							$elm$html$Html$text(' Ruin')
						]))
				]))
		]),
	U: $elm$core$String$fromChar('\u03BB'),
	P: 'Contact',
	v: 'contact'
};
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{r: nodeList, m: nodeListSize, n: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$State = $elm$core$Basics$identity;
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$initialState = $elm$core$Dict$empty;
var $author$project$Main$initialModel = F3(
	function (url, key, loadQueue) {
		return {
			an: $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$initialState,
			aR: _List_fromArray(
				[$author$project$Main$aboutme, $author$project$Main$contact]),
			c4: key,
			a0: $elm$core$Array$fromList(
				_List_fromArray(
					['0x6c', '0x6d', '0x64', '0x65', '0x78', '0x70', '0x72'])),
			a1: loadQueue,
			v: url
		};
	});
var $elm$json$Json$Decode$list = _Json_decodeList;
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$CardState = F2(
	function (visibility, height) {
		return {at: height, w: visibility};
	});
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$Shown = 3;
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$initialStateCardOpen = function (id) {
	return $elm$core$Dict$fromList(
		_List_fromArray(
			[
				_Utils_Tuple2(
				id,
				A2($EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$CardState, 3, $elm$core$Maybe$Nothing))
			]));
};
var $author$project$Main$Loaded = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$http$Http$BadUrl_ = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $elm$http$Http$NetworkError_ = {$: 2};
var $elm$http$Http$Receiving = function (a) {
	return {$: 1, a: a};
};
var $elm$http$Http$Sending = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$Timeout_ = {$: 1};
var $elm$core$Maybe$isJust = function (maybe) {
	if (!maybe.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === -1) {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === -1) {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === -1) {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (!_v0.$) {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			$elm$core$Basics$identity,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $elm$http$Http$BadBody = function (a) {
	return {$: 4, a: a};
};
var $elm$http$Http$BadStatus = function (a) {
	return {$: 3, a: a};
};
var $elm$http$Http$BadUrl = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$NetworkError = {$: 2};
var $elm$http$Http$Timeout = {$: 1};
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (!result.$) {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 0:
				var url = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadUrl(url));
			case 1:
				return $elm$core$Result$Err($elm$http$Http$Timeout);
			case 2:
				return $elm$core$Result$Err($elm$http$Http$NetworkError);
			case 3:
				var metadata = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadStatus(metadata.fe));
			default:
				var body = response.b;
				return A2(
					$elm$core$Result$mapError,
					$elm$http$Http$BadBody,
					toResult(body));
		}
	});
var $elm$http$Http$expectString = function (toMsg) {
	return A2(
		$elm$http$Http$expectStringResponse,
		toMsg,
		$elm$http$Http$resolve($elm$core$Result$Ok));
};
var $elm$http$Http$emptyBody = _Http_emptyBody;
var $elm$http$Http$Request = function (a) {
	return {$: 1, a: a};
};
var $elm$http$Http$State = F2(
	function (reqs, subs) {
		return {ec: reqs, ep: subs};
	});
var $elm$http$Http$init = $elm$core$Task$succeed(
	A2($elm$http$Http$State, $elm$core$Dict$empty, _List_Nil));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return $elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (!cmd.$) {
					var tracker = cmd.a;
					var _v2 = A2($elm$core$Dict$get, tracker, reqs);
					if (_v2.$ === 1) {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _v2.a;
						return A2(
							$elm$core$Task$andThen,
							function (_v3) {
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2($elm$core$Dict$remove, tracker, reqs));
							},
							$elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						$elm$core$Task$andThen,
						function (pid) {
							var _v4 = req.ex;
							if (_v4.$ === 1) {
								return A3($elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _v4.a;
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3($elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						$elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								$elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var $elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			$elm$core$Task$andThen,
			function (reqs) {
				return $elm$core$Task$succeed(
					A2($elm$http$Http$State, reqs, subs));
			},
			A3($elm$http$Http$updateReqs, router, cmds, state.ec));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _v0) {
		var actualTracker = _v0.a;
		var toMsg = _v0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? $elm$core$Maybe$Just(
			A2(
				$elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : $elm$core$Maybe$Nothing;
	});
var $elm$http$Http$onSelfMsg = F3(
	function (router, _v0, state) {
		var tracker = _v0.a;
		var progress = _v0.b;
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$filterMap,
					A3($elm$http$Http$maybeSend, router, tracker, progress),
					state.ep)));
	});
var $elm$http$Http$Cancel = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (!cmd.$) {
			var tracker = cmd.a;
			return $elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return $elm$http$Http$Request(
				{
					eE: r.eE,
					S: r.S,
					eO: A2(_Http_mapExpect, func, r.eO),
					eS: r.eS,
					e1: r.e1,
					fi: r.fi,
					ex: r.ex,
					v: r.v
				});
		}
	});
var $elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$http$Http$subMap = F2(
	function (func, _v0) {
		var tracker = _v0.a;
		var toMsg = _v0.b;
		return A2(
			$elm$http$Http$MySub,
			tracker,
			A2($elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager($elm$http$Http$init, $elm$http$Http$onEffects, $elm$http$Http$onSelfMsg, $elm$http$Http$cmdMap, $elm$http$Http$subMap);
var $elm$http$Http$command = _Platform_leaf('Http');
var $elm$http$Http$subscription = _Platform_leaf('Http');
var $elm$http$Http$request = function (r) {
	return $elm$http$Http$command(
		$elm$http$Http$Request(
			{eE: false, S: r.S, eO: r.eO, eS: r.eS, e1: r.e1, fi: r.fi, ex: r.ex, v: r.v}));
};
var $elm$http$Http$get = function (r) {
	return $elm$http$Http$request(
		{S: $elm$http$Http$emptyBody, eO: r.eO, eS: _List_Nil, e1: 'GET', fi: $elm$core$Maybe$Nothing, ex: $elm$core$Maybe$Nothing, v: r.v});
};
var $author$project$Main$jsonToContent = function (json) {
	return {S: _List_Nil, U: json.U, P: json.P, v: json.v};
};
var $author$project$Main$load = function (json) {
	return $elm$http$Http$get(
		{
			eO: $elm$http$Http$expectString(
				$author$project$Main$Loaded(
					$author$project$Main$jsonToContent(json))),
			v: 'https://raw.githubusercontent.com/lmdexpr/lmdexpr.com/master/Posts/' + (json.v + '.md')
		});
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $elm$url$Url$Parser$State = F5(
	function (visited, unvisited, params, frag, value) {
		return {ae: frag, ah: params, aa: unvisited, cR: value, ak: visited};
	});
var $elm$url$Url$Parser$getFirstMatch = function (states) {
	getFirstMatch:
	while (true) {
		if (!states.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var state = states.a;
			var rest = states.b;
			var _v1 = state.aa;
			if (!_v1.b) {
				return $elm$core$Maybe$Just(state.cR);
			} else {
				if ((_v1.a === '') && (!_v1.b.b)) {
					return $elm$core$Maybe$Just(state.cR);
				} else {
					var $temp$states = rest;
					states = $temp$states;
					continue getFirstMatch;
				}
			}
		}
	}
};
var $elm$url$Url$Parser$removeFinalEmpty = function (segments) {
	if (!segments.b) {
		return _List_Nil;
	} else {
		if ((segments.a === '') && (!segments.b.b)) {
			return _List_Nil;
		} else {
			var segment = segments.a;
			var rest = segments.b;
			return A2(
				$elm$core$List$cons,
				segment,
				$elm$url$Url$Parser$removeFinalEmpty(rest));
		}
	}
};
var $elm$url$Url$Parser$preparePath = function (path) {
	var _v0 = A2($elm$core$String$split, '/', path);
	if (_v0.b && (_v0.a === '')) {
		var segments = _v0.b;
		return $elm$url$Url$Parser$removeFinalEmpty(segments);
	} else {
		var segments = _v0;
		return $elm$url$Url$Parser$removeFinalEmpty(segments);
	}
};
var $elm$url$Url$Parser$addToParametersHelp = F2(
	function (value, maybeList) {
		if (maybeList.$ === 1) {
			return $elm$core$Maybe$Just(
				_List_fromArray(
					[value]));
		} else {
			var list = maybeList.a;
			return $elm$core$Maybe$Just(
				A2($elm$core$List$cons, value, list));
		}
	});
var $elm$url$Url$percentDecode = _Url_percentDecode;
var $elm$url$Url$Parser$addParam = F2(
	function (segment, dict) {
		var _v0 = A2($elm$core$String$split, '=', segment);
		if ((_v0.b && _v0.b.b) && (!_v0.b.b.b)) {
			var rawKey = _v0.a;
			var _v1 = _v0.b;
			var rawValue = _v1.a;
			var _v2 = $elm$url$Url$percentDecode(rawKey);
			if (_v2.$ === 1) {
				return dict;
			} else {
				var key = _v2.a;
				var _v3 = $elm$url$Url$percentDecode(rawValue);
				if (_v3.$ === 1) {
					return dict;
				} else {
					var value = _v3.a;
					return A3(
						$elm$core$Dict$update,
						key,
						$elm$url$Url$Parser$addToParametersHelp(value),
						dict);
				}
			}
		} else {
			return dict;
		}
	});
var $elm$url$Url$Parser$prepareQuery = function (maybeQuery) {
	if (maybeQuery.$ === 1) {
		return $elm$core$Dict$empty;
	} else {
		var qry = maybeQuery.a;
		return A3(
			$elm$core$List$foldr,
			$elm$url$Url$Parser$addParam,
			$elm$core$Dict$empty,
			A2($elm$core$String$split, '&', qry));
	}
};
var $elm$url$Url$Parser$parse = F2(
	function (_v0, url) {
		var parser = _v0;
		return $elm$url$Url$Parser$getFirstMatch(
			parser(
				A5(
					$elm$url$Url$Parser$State,
					_List_Nil,
					$elm$url$Url$Parser$preparePath(url.d1),
					$elm$url$Url$Parser$prepareQuery(url.d9),
					url.dI,
					$elm$core$Basics$identity)));
	});
var $elm$url$Url$Parser$Parser = $elm$core$Basics$identity;
var $elm$url$Url$Parser$query = function (_v0) {
	var queryParser = _v0;
	return function (_v1) {
		var visited = _v1.ak;
		var unvisited = _v1.aa;
		var params = _v1.ah;
		var frag = _v1.ae;
		var value = _v1.cR;
		return _List_fromArray(
			[
				A5(
				$elm$url$Url$Parser$State,
				visited,
				unvisited,
				params,
				frag,
				value(
					queryParser(params)))
			]);
	};
};
var $elm$url$Url$Parser$Internal$Parser = $elm$core$Basics$identity;
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $elm$url$Url$Parser$Query$custom = F2(
	function (key, func) {
		return function (dict) {
			return func(
				A2(
					$elm$core$Maybe$withDefault,
					_List_Nil,
					A2($elm$core$Dict$get, key, dict)));
		};
	});
var $elm$url$Url$Parser$Query$string = function (key) {
	return A2(
		$elm$url$Url$Parser$Query$custom,
		key,
		function (stringList) {
			if (stringList.b && (!stringList.b.b)) {
				var str = stringList.a;
				return $elm$core$Maybe$Just(str);
			} else {
				return $elm$core$Maybe$Nothing;
			}
		});
};
var $author$project$Main$loadNext = function (model) {
	var _v0 = model.a1;
	if (_v0.b) {
		var hd = _v0.a;
		var tl = _v0.b;
		return _Utils_Tuple2(
			_Utils_update(
				model,
				{a1: tl}),
			$author$project$Main$load(hd));
	} else {
		var _v1 = A2(
			$elm$url$Url$Parser$parse,
			$elm$url$Url$Parser$query(
				$elm$url$Url$Parser$Query$string('content')),
			model.v);
		if ((!_v1.$) && (!_v1.a.$)) {
			var cid = _v1.a.a;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						an: $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$initialStateCardOpen(cid)
					}),
				$elm$core$Platform$Cmd$none);
		} else {
			return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	}
};
var $elm$core$Result$withDefault = F2(
	function (def, result) {
		if (!result.$) {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var $author$project$Main$init = F3(
	function (v, url, key) {
		return $author$project$Main$loadNext(
			A3(
				$author$project$Main$initialModel,
				url,
				key,
				A2(
					$elm$core$Result$withDefault,
					_List_Nil,
					A2(
						$elm$json$Json$Decode$decodeValue,
						$elm$json$Json$Decode$list($author$project$Main$decodeOfJsonContent),
						v))));
	});
var $author$project$Main$AccordionMsg = function (a) {
	return {$: 1, a: a};
};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$Hidden = 0;
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$StartDown = 1;
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$StartUp = 2;
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2($elm$core$Dict$map, func, left),
				A2($elm$core$Dict$map, func, right));
		}
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $elm$browser$Browser$AnimationManager$Time = function (a) {
	return {$: 0, a: a};
};
var $elm$browser$Browser$AnimationManager$State = F3(
	function (subs, request, oldTime) {
		return {db: oldTime, ed: request, ep: subs};
	});
var $elm$browser$Browser$AnimationManager$init = $elm$core$Task$succeed(
	A3($elm$browser$Browser$AnimationManager$State, _List_Nil, $elm$core$Maybe$Nothing, 0));
var $elm$browser$Browser$AnimationManager$now = _Browser_now(0);
var $elm$browser$Browser$AnimationManager$rAF = _Browser_rAF(0);
var $elm$browser$Browser$AnimationManager$onEffects = F3(
	function (router, subs, _v0) {
		var request = _v0.ed;
		var oldTime = _v0.db;
		var _v1 = _Utils_Tuple2(request, subs);
		if (_v1.a.$ === 1) {
			if (!_v1.b.b) {
				var _v2 = _v1.a;
				return $elm$browser$Browser$AnimationManager$init;
			} else {
				var _v4 = _v1.a;
				return A2(
					$elm$core$Task$andThen,
					function (pid) {
						return A2(
							$elm$core$Task$andThen,
							function (time) {
								return $elm$core$Task$succeed(
									A3(
										$elm$browser$Browser$AnimationManager$State,
										subs,
										$elm$core$Maybe$Just(pid),
										time));
							},
							$elm$browser$Browser$AnimationManager$now);
					},
					$elm$core$Process$spawn(
						A2(
							$elm$core$Task$andThen,
							$elm$core$Platform$sendToSelf(router),
							$elm$browser$Browser$AnimationManager$rAF)));
			}
		} else {
			if (!_v1.b.b) {
				var pid = _v1.a.a;
				return A2(
					$elm$core$Task$andThen,
					function (_v3) {
						return $elm$browser$Browser$AnimationManager$init;
					},
					$elm$core$Process$kill(pid));
			} else {
				return $elm$core$Task$succeed(
					A3($elm$browser$Browser$AnimationManager$State, subs, request, oldTime));
			}
		}
	});
var $elm$time$Time$Posix = $elm$core$Basics$identity;
var $elm$time$Time$millisToPosix = $elm$core$Basics$identity;
var $elm$browser$Browser$AnimationManager$onSelfMsg = F3(
	function (router, newTime, _v0) {
		var subs = _v0.ep;
		var oldTime = _v0.db;
		var send = function (sub) {
			if (!sub.$) {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(
						$elm$time$Time$millisToPosix(newTime)));
			} else {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(newTime - oldTime));
			}
		};
		return A2(
			$elm$core$Task$andThen,
			function (pid) {
				return A2(
					$elm$core$Task$andThen,
					function (_v1) {
						return $elm$core$Task$succeed(
							A3(
								$elm$browser$Browser$AnimationManager$State,
								subs,
								$elm$core$Maybe$Just(pid),
								newTime));
					},
					$elm$core$Task$sequence(
						A2($elm$core$List$map, send, subs)));
			},
			$elm$core$Process$spawn(
				A2(
					$elm$core$Task$andThen,
					$elm$core$Platform$sendToSelf(router),
					$elm$browser$Browser$AnimationManager$rAF)));
	});
var $elm$browser$Browser$AnimationManager$Delta = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$browser$Browser$AnimationManager$subMap = F2(
	function (func, sub) {
		if (!sub.$) {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Time(
				A2($elm$core$Basics$composeL, func, tagger));
		} else {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Delta(
				A2($elm$core$Basics$composeL, func, tagger));
		}
	});
_Platform_effectManagers['Browser.AnimationManager'] = _Platform_createManager($elm$browser$Browser$AnimationManager$init, $elm$browser$Browser$AnimationManager$onEffects, $elm$browser$Browser$AnimationManager$onSelfMsg, 0, $elm$browser$Browser$AnimationManager$subMap);
var $elm$browser$Browser$AnimationManager$subscription = _Platform_leaf('Browser.AnimationManager');
var $elm$browser$Browser$AnimationManager$onAnimationFrame = function (tagger) {
	return $elm$browser$Browser$AnimationManager$subscription(
		$elm$browser$Browser$AnimationManager$Time(tagger));
};
var $elm$browser$Browser$Events$onAnimationFrame = $elm$browser$Browser$AnimationManager$onAnimationFrame;
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$subscriptions = F2(
	function (_v0, toMsg) {
		var cardStates = _v0;
		var updState = A2(
			$elm$core$Dict$map,
			F2(
				function (id, state) {
					var _v3 = state.w;
					switch (_v3) {
						case 1:
							return _Utils_update(
								state,
								{w: 3});
						case 2:
							return _Utils_update(
								state,
								{w: 0});
						default:
							return state;
					}
				}),
			cardStates);
		var needsSub = A2(
			$elm$core$List$any,
			function (_v2) {
				var state = _v2.b;
				return A2(
					$elm$core$List$member,
					state.w,
					_List_fromArray(
						[1, 2]));
			},
			$elm$core$Dict$toList(cardStates));
		return needsSub ? $elm$browser$Browser$Events$onAnimationFrame(
			function (_v1) {
				return toMsg(updState);
			}) : $elm$core$Platform$Sub$none;
	});
var $author$project$Main$subscriptions = function (model) {
	return A2($EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$subscriptions, model.an, $author$project$Main$AccordionMsg);
};
var $elm$core$Char$fromCode = _Char_fromCode;
var $elm$core$Basics$pow = _Basics_pow;
var $rtfeldman$elm_hex$Hex$fromStringHelp = F3(
	function (position, chars, accumulated) {
		fromStringHelp:
		while (true) {
			if (!chars.b) {
				return $elm$core$Result$Ok(accumulated);
			} else {
				var _char = chars.a;
				var rest = chars.b;
				switch (_char) {
					case '0':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated;
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '1':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + A2($elm$core$Basics$pow, 16, position);
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '2':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (2 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '3':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (3 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '4':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (4 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '5':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (5 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '6':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (6 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '7':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (7 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '8':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (8 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case '9':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (9 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'a':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (10 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'b':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (11 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'c':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (12 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'd':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (13 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'e':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (14 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					case 'f':
						var $temp$position = position - 1,
							$temp$chars = rest,
							$temp$accumulated = accumulated + (15 * A2($elm$core$Basics$pow, 16, position));
						position = $temp$position;
						chars = $temp$chars;
						accumulated = $temp$accumulated;
						continue fromStringHelp;
					default:
						var nonHex = _char;
						return $elm$core$Result$Err(
							$elm$core$String$fromChar(nonHex) + ' is not a valid hexadecimal character.');
				}
			}
		}
	});
var $elm$core$Result$map = F2(
	function (func, ra) {
		if (!ra.$) {
			var a = ra.a;
			return $elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return $elm$core$Result$Err(e);
		}
	});
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(xs);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $rtfeldman$elm_hex$Hex$fromString = function (str) {
	if ($elm$core$String$isEmpty(str)) {
		return $elm$core$Result$Err('Empty strings are not valid hexadecimal strings.');
	} else {
		var result = function () {
			if (A2($elm$core$String$startsWith, '-', str)) {
				var list = A2(
					$elm$core$Maybe$withDefault,
					_List_Nil,
					$elm$core$List$tail(
						$elm$core$String$toList(str)));
				return A2(
					$elm$core$Result$map,
					$elm$core$Basics$negate,
					A3(
						$rtfeldman$elm_hex$Hex$fromStringHelp,
						$elm$core$List$length(list) - 1,
						list,
						0));
			} else {
				return A3(
					$rtfeldman$elm_hex$Hex$fromStringHelp,
					$elm$core$String$length(str) - 1,
					$elm$core$String$toList(str),
					0);
			}
		}();
		var formatError = function (err) {
			return A2(
				$elm$core$String$join,
				' ',
				_List_fromArray(
					['\"' + (str + '\"'), 'is not a valid hexadecimal string because', err]));
		};
		return A2($elm$core$Result$mapError, formatError, result);
	}
};
var $elm$core$String$fromList = _String_fromList;
var $elm$core$Basics$modBy = _Basics_modBy;
var $rtfeldman$elm_hex$Hex$unsafeToDigit = function (num) {
	unsafeToDigit:
	while (true) {
		switch (num) {
			case 0:
				return '0';
			case 1:
				return '1';
			case 2:
				return '2';
			case 3:
				return '3';
			case 4:
				return '4';
			case 5:
				return '5';
			case 6:
				return '6';
			case 7:
				return '7';
			case 8:
				return '8';
			case 9:
				return '9';
			case 10:
				return 'a';
			case 11:
				return 'b';
			case 12:
				return 'c';
			case 13:
				return 'd';
			case 14:
				return 'e';
			case 15:
				return 'f';
			default:
				var $temp$num = num;
				num = $temp$num;
				continue unsafeToDigit;
		}
	}
};
var $rtfeldman$elm_hex$Hex$unsafePositiveToDigits = F2(
	function (digits, num) {
		unsafePositiveToDigits:
		while (true) {
			if (num < 16) {
				return A2(
					$elm$core$List$cons,
					$rtfeldman$elm_hex$Hex$unsafeToDigit(num),
					digits);
			} else {
				var $temp$digits = A2(
					$elm$core$List$cons,
					$rtfeldman$elm_hex$Hex$unsafeToDigit(
						A2($elm$core$Basics$modBy, 16, num)),
					digits),
					$temp$num = (num / 16) | 0;
				digits = $temp$digits;
				num = $temp$num;
				continue unsafePositiveToDigits;
			}
		}
	});
var $rtfeldman$elm_hex$Hex$toString = function (num) {
	return $elm$core$String$fromList(
		(num < 0) ? A2(
			$elm$core$List$cons,
			'-',
			A2($rtfeldman$elm_hex$Hex$unsafePositiveToDigits, _List_Nil, -num)) : A2($rtfeldman$elm_hex$Hex$unsafePositiveToDigits, _List_Nil, num));
};
var $author$project$Main$convert = function (target) {
	if (A2($elm$core$String$startsWith, '0x', target)) {
		var _v0 = $rtfeldman$elm_hex$Hex$fromString(
			A3($elm$core$String$slice, 2, 4, target));
		if (!_v0.$) {
			var n = _v0.a;
			return $elm$core$String$fromChar(
				$elm$core$Char$fromCode(n));
		} else {
			return 'ERROR : hex to char';
		}
	} else {
		var _v1 = $elm$core$String$uncons(target);
		if (!_v1.$) {
			var _v2 = _v1.a;
			var c = _v2.a;
			return function (s) {
				return '0x' + s;
			}(
				$rtfeldman$elm_hex$Hex$toString(
					$elm$core$Char$toCode(c)));
		} else {
			return 'ERROR : char to hex';
		}
	}
};
var $elm$browser$Browser$Navigation$load = _Browser_load;
var $elm$browser$Browser$Navigation$pushUrl = _Browser_pushUrl;
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var $elm$core$Array$setHelp = F4(
	function (shift, index, value, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
		if (!_v0.$) {
			var subTree = _v0.a;
			var newSub = A4($elm$core$Array$setHelp, shift - $elm$core$Array$shiftStep, index, value, subTree);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$SubTree(newSub),
				tree);
		} else {
			var values = _v0.a;
			var newLeaf = A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, values);
			return A3(
				$elm$core$Elm$JsArray$unsafeSet,
				pos,
				$elm$core$Array$Leaf(newLeaf),
				tree);
		}
	});
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$set = F3(
	function (index, value, array) {
		var len = array.a;
		var startShift = array.b;
		var tree = array.c;
		var tail = array.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? array : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			tree,
			A3($elm$core$Elm$JsArray$unsafeSet, $elm$core$Array$bitMask & index, value, tail)) : A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A4($elm$core$Array$setHelp, startShift, index, value, tree),
			tail));
	});
var $author$project$Main$errorToString = function (error) {
	switch (error.$) {
		case 0:
			var url = error.a;
			return 'The URL ' + (url + ' was invalid');
		case 1:
			return 'Unable to reach the server, try again';
		case 2:
			return 'Unable to reach the server, check your network connection';
		case 3:
			switch (error.a) {
				case 500:
					return 'The server had a problem, try again later';
				case 400:
					return 'Verify your information and try again';
				default:
					return 'Unknown error';
			}
		default:
			var errorMessage = error.a;
			return errorMessage;
	}
};
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $pablohirafuji$elm_markdown$Markdown$Block$BlockQuote = function (a) {
	return {$: 5, a: a};
};
var $pablohirafuji$elm_markdown$Markdown$Block$List = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var $pablohirafuji$elm_markdown$Markdown$Block$Paragraph = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $elm$core$String$right = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(
			$elm$core$String$slice,
			-n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$trim = _String_trim;
var $pablohirafuji$elm_markdown$Markdown$Block$formatParagraphLine = function (rawParagraph) {
	return (A2($elm$core$String$right, 2, rawParagraph) === '  ') ? ($elm$core$String$trim(rawParagraph) + '  ') : $elm$core$String$trim(rawParagraph);
};
var $pablohirafuji$elm_markdown$Markdown$Block$addToParagraph = F2(
	function (paragraph, rawLine) {
		return A2(
			$pablohirafuji$elm_markdown$Markdown$Block$Paragraph,
			paragraph + ('\n' + $pablohirafuji$elm_markdown$Markdown$Block$formatParagraphLine(rawLine)),
			_List_Nil);
	});
var $elm$core$Basics$not = _Basics_not;
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $elm$regex$Regex$Match = F4(
	function (match, index, number, submatches) {
		return {eY: index, a2: match, e2: number, eo: submatches};
	});
var $elm$regex$Regex$fromStringWith = _Regex_fromStringWith;
var $elm$regex$Regex$fromString = function (string) {
	return A2(
		$elm$regex$Regex$fromStringWith,
		{du: false, dZ: false},
		string);
};
var $elm$regex$Regex$never = _Regex_never;
var $pablohirafuji$elm_markdown$Markdown$Block$blockQuoteLineRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^ {0,3}(?:>[ ]?)(.*)$'));
var $pablohirafuji$elm_markdown$Markdown$Block$blankLineRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^\\s*$'));
var $elm$regex$Regex$contains = _Regex_contains;
var $pablohirafuji$elm_markdown$Markdown$Block$calcListIndentLength = function (_v0) {
	var listBlock = _v0.a;
	var indentSpace = _v0.b;
	var rawLine = _v0.c;
	var indentSpaceLength = $elm$core$String$length(indentSpace);
	var isIndentedCode = indentSpaceLength >= 4;
	var updtRawLine = isIndentedCode ? _Utils_ap(indentSpace, rawLine) : rawLine;
	var indentLength = (isIndentedCode || A2($elm$regex$Regex$contains, $pablohirafuji$elm_markdown$Markdown$Block$blankLineRegex, rawLine)) ? (listBlock.u - indentSpaceLength) : listBlock.u;
	return _Utils_Tuple2(
		_Utils_update(
			listBlock,
			{u: indentLength}),
		updtRawLine);
};
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (!maybeValue.$) {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $pablohirafuji$elm_markdown$Markdown$Block$atxHeadingLineRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^ {0,3}(#{1,6})' + ('(?:[ \\t]+[ \\t#]+$|[ \\t]+|$)' + '(.*?)(?:\\s+[ \\t#]*)?$')));
var $pablohirafuji$elm_markdown$Markdown$Block$Heading = F3(
	function (a, b, c) {
		return {$: 2, a: a, b: b, c: c};
	});
var $pablohirafuji$elm_markdown$Markdown$Block$extractATXHeadingRM = function (match) {
	var _v0 = match.eo;
	if ((_v0.b && (!_v0.a.$)) && _v0.b.b) {
		var lvl = _v0.a.a;
		var _v1 = _v0.b;
		var maybeHeading = _v1.a;
		return $elm$core$Maybe$Just(
			A3(
				$pablohirafuji$elm_markdown$Markdown$Block$Heading,
				A2($elm$core$Maybe$withDefault, '', maybeHeading),
				$elm$core$String$length(lvl),
				_List_Nil));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$regex$Regex$findAtMost = _Regex_findAtMost;
var $elm$core$Result$fromMaybe = F2(
	function (err, maybe) {
		if (!maybe.$) {
			var v = maybe.a;
			return $elm$core$Result$Ok(v);
		} else {
			return $elm$core$Result$Err(err);
		}
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $pablohirafuji$elm_markdown$Markdown$Block$checkATXHeadingLine = function (_v0) {
	var rawLine = _v0.a;
	var ast = _v0.b;
	return A2(
		$elm$core$Result$fromMaybe,
		_Utils_Tuple2(rawLine, ast),
		A2(
			$elm$core$Maybe$map,
			function (a) {
				return A2($elm$core$List$cons, a, ast);
			},
			A2(
				$elm$core$Maybe$andThen,
				$pablohirafuji$elm_markdown$Markdown$Block$extractATXHeadingRM,
				$elm$core$List$head(
					A3($elm$regex$Regex$findAtMost, 1, $pablohirafuji$elm_markdown$Markdown$Block$atxHeadingLineRegex, rawLine)))));
};
var $pablohirafuji$elm_markdown$Markdown$Block$BlankLine = function (a) {
	return {$: 0, a: a};
};
var $pablohirafuji$elm_markdown$Markdown$Block$CodeBlock = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $pablohirafuji$elm_markdown$Markdown$Block$Fenced = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $pablohirafuji$elm_markdown$Markdown$Block$addBlankLineToListBlock = F2(
	function (match, asts) {
		if (!asts.b) {
			return _List_fromArray(
				[
					_List_fromArray(
					[
						$pablohirafuji$elm_markdown$Markdown$Block$BlankLine(match.a2)
					])
				]);
		} else {
			var ast = asts.a;
			var astsTail = asts.b;
			return A2(
				$elm$core$List$cons,
				A2($pablohirafuji$elm_markdown$Markdown$Block$parseBlankLine, ast, match),
				astsTail);
		}
	});
var $pablohirafuji$elm_markdown$Markdown$Block$parseBlankLine = F2(
	function (ast, match) {
		_v0$2:
		while (true) {
			if (ast.b) {
				switch (ast.a.$) {
					case 3:
						if ((ast.a.a.$ === 1) && ast.a.a.a) {
							var _v1 = ast.a;
							var _v2 = _v1.a;
							var fence = _v2.b;
							var code = _v1.b;
							var astTail = ast.b;
							return function (a) {
								return A2($elm$core$List$cons, a, astTail);
							}(
								A2(
									$pablohirafuji$elm_markdown$Markdown$Block$CodeBlock,
									A2($pablohirafuji$elm_markdown$Markdown$Block$Fenced, true, fence),
									code + '\n'));
						} else {
							break _v0$2;
						}
					case 6:
						var _v3 = ast.a;
						var model = _v3.a;
						var items = _v3.b;
						var astTail = ast.b;
						return A2(
							$elm$core$List$cons,
							A2(
								$pablohirafuji$elm_markdown$Markdown$Block$List,
								model,
								A2($pablohirafuji$elm_markdown$Markdown$Block$addBlankLineToListBlock, match, items)),
							astTail);
					default:
						break _v0$2;
				}
			} else {
				break _v0$2;
			}
		}
		return A2(
			$elm$core$List$cons,
			$pablohirafuji$elm_markdown$Markdown$Block$BlankLine(match.a2),
			ast);
	});
var $pablohirafuji$elm_markdown$Markdown$Block$checkBlankLine = function (_v0) {
	var rawLine = _v0.a;
	var ast = _v0.b;
	return A2(
		$elm$core$Result$fromMaybe,
		_Utils_Tuple2(rawLine, ast),
		A2(
			$elm$core$Maybe$map,
			$pablohirafuji$elm_markdown$Markdown$Block$parseBlankLine(ast),
			$elm$core$List$head(
				A3($elm$regex$Regex$findAtMost, 1, $pablohirafuji$elm_markdown$Markdown$Block$blankLineRegex, rawLine))));
};
var $pablohirafuji$elm_markdown$Markdown$Block$indentedCodeLineRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^(?: {4,4}| {0,3}\\t)(.*)$'));
var $pablohirafuji$elm_markdown$Markdown$Block$Indented = {$: 0};
var $pablohirafuji$elm_markdown$Markdown$Block$blocksAfterBlankLines = F2(
	function (ast, blankLines) {
		blocksAfterBlankLines:
		while (true) {
			if (ast.b && (!ast.a.$)) {
				var blankStr = ast.a.a;
				var astTail = ast.b;
				var $temp$ast = astTail,
					$temp$blankLines = A2($elm$core$List$cons, blankStr, blankLines);
				ast = $temp$ast;
				blankLines = $temp$blankLines;
				continue blocksAfterBlankLines;
			} else {
				return _Utils_Tuple2(ast, blankLines);
			}
		}
	});
var $pablohirafuji$elm_markdown$Markdown$Block$maybeContinueParagraph = F2(
	function (rawLine, ast) {
		_v0$3:
		while (true) {
			if (ast.b) {
				switch (ast.a.$) {
					case 4:
						var _v1 = ast.a;
						var paragraph = _v1.a;
						var astTail = ast.b;
						return $elm$core$Maybe$Just(
							A2(
								$elm$core$List$cons,
								A2($pablohirafuji$elm_markdown$Markdown$Block$addToParagraph, paragraph, rawLine),
								astTail));
					case 5:
						var bqAST = ast.a.a;
						var astTail = ast.b;
						return A2(
							$elm$core$Maybe$map,
							function (updtBqAST) {
								return A2(
									$elm$core$List$cons,
									$pablohirafuji$elm_markdown$Markdown$Block$BlockQuote(updtBqAST),
									astTail);
							},
							A2($pablohirafuji$elm_markdown$Markdown$Block$maybeContinueParagraph, rawLine, bqAST));
					case 6:
						var _v2 = ast.a;
						var model = _v2.a;
						var items = _v2.b;
						var astTail = ast.b;
						if (items.b) {
							var itemAST = items.a;
							var itemASTTail = items.b;
							return A2(
								$elm$core$Maybe$map,
								A2(
									$elm$core$Basics$composeR,
									function (a) {
										return A2($elm$core$List$cons, a, itemASTTail);
									},
									A2(
										$elm$core$Basics$composeR,
										$pablohirafuji$elm_markdown$Markdown$Block$List(model),
										function (a) {
											return A2($elm$core$List$cons, a, astTail);
										})),
								A2($pablohirafuji$elm_markdown$Markdown$Block$maybeContinueParagraph, rawLine, itemAST));
						} else {
							return $elm$core$Maybe$Nothing;
						}
					default:
						break _v0$3;
				}
			} else {
				break _v0$3;
			}
		}
		return $elm$core$Maybe$Nothing;
	});
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $elm$regex$Regex$replace = _Regex_replaceAtMost(_Regex_infinity);
var $elm$regex$Regex$replaceAtMost = _Regex_replaceAtMost;
var $pablohirafuji$elm_markdown$Markdown$Helpers$tabRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('\\t'));
var $pablohirafuji$elm_markdown$Markdown$Helpers$indentLine = function (indentLength_) {
	return A2(
		$elm$core$Basics$composeR,
		A2(
			$elm$regex$Regex$replace,
			$pablohirafuji$elm_markdown$Markdown$Helpers$tabRegex,
			function (_v0) {
				return '    ';
			}),
		A3(
			$elm$regex$Regex$replaceAtMost,
			1,
			A2(
				$elm$core$Maybe$withDefault,
				$elm$regex$Regex$never,
				$elm$regex$Regex$fromString(
					'^ {0,' + ($elm$core$String$fromInt(indentLength_) + '}'))),
			function (_v1) {
				return '';
			}));
};
var $pablohirafuji$elm_markdown$Markdown$Block$resumeIndentedCodeBlock = F2(
	function (codeLine, _v0) {
		var remainBlocks = _v0.a;
		var blankLines = _v0.b;
		if ((remainBlocks.b && (remainBlocks.a.$ === 3)) && (!remainBlocks.a.a.$)) {
			var _v2 = remainBlocks.a;
			var _v3 = _v2.a;
			var codeStr = _v2.b;
			var remainBlocksTail = remainBlocks.b;
			return $elm$core$Maybe$Just(
				function (a) {
					return A2($elm$core$List$cons, a, remainBlocksTail);
				}(
					A2(
						$pablohirafuji$elm_markdown$Markdown$Block$CodeBlock,
						$pablohirafuji$elm_markdown$Markdown$Block$Indented,
						function (a) {
							return a + (codeLine + '\n');
						}(
							_Utils_ap(
								codeStr,
								$elm$core$String$concat(
									A2(
										$elm$core$List$map,
										function (bl) {
											return A2($pablohirafuji$elm_markdown$Markdown$Helpers$indentLine, 4, bl) + '\n';
										},
										blankLines)))))));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $pablohirafuji$elm_markdown$Markdown$Block$parseIndentedCodeLine = F2(
	function (ast, codeLine) {
		_v0$2:
		while (true) {
			if (ast.b) {
				switch (ast.a.$) {
					case 3:
						if (!ast.a.a.$) {
							var _v1 = ast.a;
							var _v2 = _v1.a;
							var codeStr = _v1.b;
							var astTail = ast.b;
							return function (a) {
								return A2($elm$core$List$cons, a, astTail);
							}(
								A2($pablohirafuji$elm_markdown$Markdown$Block$CodeBlock, $pablohirafuji$elm_markdown$Markdown$Block$Indented, codeStr + (codeLine + '\n')));
						} else {
							break _v0$2;
						}
					case 0:
						var blankStr = ast.a.a;
						var astTail = ast.b;
						return A2(
							$elm$core$Maybe$withDefault,
							function (a) {
								return A2($elm$core$List$cons, a, ast);
							}(
								A2($pablohirafuji$elm_markdown$Markdown$Block$CodeBlock, $pablohirafuji$elm_markdown$Markdown$Block$Indented, codeLine + '\n')),
							A2(
								$pablohirafuji$elm_markdown$Markdown$Block$resumeIndentedCodeBlock,
								codeLine,
								A2(
									$pablohirafuji$elm_markdown$Markdown$Block$blocksAfterBlankLines,
									astTail,
									_List_fromArray(
										[blankStr]))));
					default:
						break _v0$2;
				}
			} else {
				break _v0$2;
			}
		}
		return A2(
			$elm$core$Maybe$withDefault,
			function (a) {
				return A2($elm$core$List$cons, a, ast);
			}(
				A2($pablohirafuji$elm_markdown$Markdown$Block$CodeBlock, $pablohirafuji$elm_markdown$Markdown$Block$Indented, codeLine + '\n')),
			A2($pablohirafuji$elm_markdown$Markdown$Block$maybeContinueParagraph, codeLine, ast));
	});
var $pablohirafuji$elm_markdown$Markdown$Block$checkIndentedCode = function (_v0) {
	var rawLine = _v0.a;
	var ast = _v0.b;
	return A2(
		$elm$core$Result$fromMaybe,
		_Utils_Tuple2(rawLine, ast),
		A2(
			$elm$core$Maybe$map,
			$pablohirafuji$elm_markdown$Markdown$Block$parseIndentedCodeLine(ast),
			A2(
				$elm$core$Maybe$withDefault,
				$elm$core$Maybe$Nothing,
				A2(
					$elm$core$Maybe$withDefault,
					$elm$core$Maybe$Nothing,
					A2(
						$elm$core$Maybe$map,
						A2(
							$elm$core$Basics$composeR,
							function ($) {
								return $.eo;
							},
							$elm$core$List$head),
						$elm$core$List$head(
							A3($elm$regex$Regex$findAtMost, 1, $pablohirafuji$elm_markdown$Markdown$Block$indentedCodeLineRegex, rawLine)))))));
};
var $pablohirafuji$elm_markdown$Markdown$Entity$decimalRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('&#([0-9]{1,8});'));
var $pablohirafuji$elm_markdown$Markdown$Entity$isBadEndUnicode = function (_int) {
	var remain_ = A2($elm$core$Basics$modBy, 16, _int);
	var remain = A2($elm$core$Basics$modBy, 131070, _int);
	return (_int >= 131070) && ((((0 <= remain) && (remain <= 15)) || ((65536 <= remain) && (remain <= 65551))) && ((remain_ === 14) || (remain_ === 15)));
};
var $pablohirafuji$elm_markdown$Markdown$Entity$isValidUnicode = function (_int) {
	return (_int === 9) || ((_int === 10) || ((_int === 13) || ((_int === 133) || (((32 <= _int) && (_int <= 126)) || (((160 <= _int) && (_int <= 55295)) || (((57344 <= _int) && (_int <= 64975)) || (((65008 <= _int) && (_int <= 65533)) || ((65536 <= _int) && (_int <= 1114109)))))))));
};
var $pablohirafuji$elm_markdown$Markdown$Entity$validUnicode = function (_int) {
	return ($pablohirafuji$elm_markdown$Markdown$Entity$isValidUnicode(_int) && (!$pablohirafuji$elm_markdown$Markdown$Entity$isBadEndUnicode(_int))) ? $elm$core$String$fromChar(
		$elm$core$Char$fromCode(_int)) : $elm$core$String$fromChar(
		$elm$core$Char$fromCode(65533));
};
var $pablohirafuji$elm_markdown$Markdown$Entity$replaceDecimal = function (match) {
	return A2(
		$elm$core$Maybe$withDefault,
		match.a2,
		A2(
			$elm$core$Maybe$map,
			$pablohirafuji$elm_markdown$Markdown$Entity$validUnicode,
			A2(
				$elm$core$Maybe$andThen,
				$elm$core$String$toInt,
				A2(
					$elm$core$Maybe$withDefault,
					$elm$core$Maybe$Nothing,
					$elm$core$List$head(match.eo)))));
};
var $pablohirafuji$elm_markdown$Markdown$Entity$replaceDecimals = A2($elm$regex$Regex$replace, $pablohirafuji$elm_markdown$Markdown$Entity$decimalRegex, $pablohirafuji$elm_markdown$Markdown$Entity$replaceDecimal);
var $pablohirafuji$elm_markdown$Markdown$Entity$entitiesRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('&([0-9a-zA-Z]+);'));
var $pablohirafuji$elm_markdown$Markdown$Entity$entities = $elm$core$Dict$fromList(
	_List_fromArray(
		[
			_Utils_Tuple2('quot', 34),
			_Utils_Tuple2('amp', 38),
			_Utils_Tuple2('apos', 39),
			_Utils_Tuple2('lt', 60),
			_Utils_Tuple2('gt', 62),
			_Utils_Tuple2('nbsp', 160),
			_Utils_Tuple2('iexcl', 161),
			_Utils_Tuple2('cent', 162),
			_Utils_Tuple2('pound', 163),
			_Utils_Tuple2('curren', 164),
			_Utils_Tuple2('yen', 165),
			_Utils_Tuple2('brvbar', 166),
			_Utils_Tuple2('sect', 167),
			_Utils_Tuple2('uml', 168),
			_Utils_Tuple2('copy', 169),
			_Utils_Tuple2('ordf', 170),
			_Utils_Tuple2('laquo', 171),
			_Utils_Tuple2('not', 172),
			_Utils_Tuple2('shy', 173),
			_Utils_Tuple2('reg', 174),
			_Utils_Tuple2('macr', 175),
			_Utils_Tuple2('deg', 176),
			_Utils_Tuple2('plusmn', 177),
			_Utils_Tuple2('sup2', 178),
			_Utils_Tuple2('sup3', 179),
			_Utils_Tuple2('acute', 180),
			_Utils_Tuple2('micro', 181),
			_Utils_Tuple2('para', 182),
			_Utils_Tuple2('middot', 183),
			_Utils_Tuple2('cedil', 184),
			_Utils_Tuple2('sup1', 185),
			_Utils_Tuple2('ordm', 186),
			_Utils_Tuple2('raquo', 187),
			_Utils_Tuple2('frac14', 188),
			_Utils_Tuple2('frac12', 189),
			_Utils_Tuple2('frac34', 190),
			_Utils_Tuple2('iquest', 191),
			_Utils_Tuple2('Agrave', 192),
			_Utils_Tuple2('Aacute', 193),
			_Utils_Tuple2('Acirc', 194),
			_Utils_Tuple2('Atilde', 195),
			_Utils_Tuple2('Auml', 196),
			_Utils_Tuple2('Aring', 197),
			_Utils_Tuple2('AElig', 198),
			_Utils_Tuple2('Ccedil', 199),
			_Utils_Tuple2('Egrave', 200),
			_Utils_Tuple2('Eacute', 201),
			_Utils_Tuple2('Ecirc', 202),
			_Utils_Tuple2('Euml', 203),
			_Utils_Tuple2('Igrave', 204),
			_Utils_Tuple2('Iacute', 205),
			_Utils_Tuple2('Icirc', 206),
			_Utils_Tuple2('Iuml', 207),
			_Utils_Tuple2('ETH', 208),
			_Utils_Tuple2('Ntilde', 209),
			_Utils_Tuple2('Ograve', 210),
			_Utils_Tuple2('Oacute', 211),
			_Utils_Tuple2('Ocirc', 212),
			_Utils_Tuple2('Otilde', 213),
			_Utils_Tuple2('Ouml', 214),
			_Utils_Tuple2('times', 215),
			_Utils_Tuple2('Oslash', 216),
			_Utils_Tuple2('Ugrave', 217),
			_Utils_Tuple2('Uacute', 218),
			_Utils_Tuple2('Ucirc', 219),
			_Utils_Tuple2('Uuml', 220),
			_Utils_Tuple2('Yacute', 221),
			_Utils_Tuple2('THORN', 222),
			_Utils_Tuple2('szlig', 223),
			_Utils_Tuple2('agrave', 224),
			_Utils_Tuple2('aacute', 225),
			_Utils_Tuple2('acirc', 226),
			_Utils_Tuple2('atilde', 227),
			_Utils_Tuple2('auml', 228),
			_Utils_Tuple2('aring', 229),
			_Utils_Tuple2('aelig', 230),
			_Utils_Tuple2('ccedil', 231),
			_Utils_Tuple2('egrave', 232),
			_Utils_Tuple2('eacute', 233),
			_Utils_Tuple2('ecirc', 234),
			_Utils_Tuple2('euml', 235),
			_Utils_Tuple2('igrave', 236),
			_Utils_Tuple2('iacute', 237),
			_Utils_Tuple2('icirc', 238),
			_Utils_Tuple2('iuml', 239),
			_Utils_Tuple2('eth', 240),
			_Utils_Tuple2('ntilde', 241),
			_Utils_Tuple2('ograve', 242),
			_Utils_Tuple2('oacute', 243),
			_Utils_Tuple2('ocirc', 244),
			_Utils_Tuple2('otilde', 245),
			_Utils_Tuple2('ouml', 246),
			_Utils_Tuple2('divide', 247),
			_Utils_Tuple2('oslash', 248),
			_Utils_Tuple2('ugrave', 249),
			_Utils_Tuple2('uacute', 250),
			_Utils_Tuple2('ucirc', 251),
			_Utils_Tuple2('uuml', 252),
			_Utils_Tuple2('yacute', 253),
			_Utils_Tuple2('thorn', 254),
			_Utils_Tuple2('yuml', 255),
			_Utils_Tuple2('OElig', 338),
			_Utils_Tuple2('oelig', 339),
			_Utils_Tuple2('Scaron', 352),
			_Utils_Tuple2('scaron', 353),
			_Utils_Tuple2('Yuml', 376),
			_Utils_Tuple2('fnof', 402),
			_Utils_Tuple2('circ', 710),
			_Utils_Tuple2('tilde', 732),
			_Utils_Tuple2('Alpha', 913),
			_Utils_Tuple2('Beta', 914),
			_Utils_Tuple2('Gamma', 915),
			_Utils_Tuple2('Delta', 916),
			_Utils_Tuple2('Epsilon', 917),
			_Utils_Tuple2('Zeta', 918),
			_Utils_Tuple2('Eta', 919),
			_Utils_Tuple2('Theta', 920),
			_Utils_Tuple2('Iota', 921),
			_Utils_Tuple2('Kappa', 922),
			_Utils_Tuple2('Lambda', 923),
			_Utils_Tuple2('Mu', 924),
			_Utils_Tuple2('Nu', 925),
			_Utils_Tuple2('Xi', 926),
			_Utils_Tuple2('Omicron', 927),
			_Utils_Tuple2('Pi', 928),
			_Utils_Tuple2('Rho', 929),
			_Utils_Tuple2('Sigma', 931),
			_Utils_Tuple2('Tau', 932),
			_Utils_Tuple2('Upsilon', 933),
			_Utils_Tuple2('Phi', 934),
			_Utils_Tuple2('Chi', 935),
			_Utils_Tuple2('Psi', 936),
			_Utils_Tuple2('Omega', 937),
			_Utils_Tuple2('alpha', 945),
			_Utils_Tuple2('beta', 946),
			_Utils_Tuple2('gamma', 947),
			_Utils_Tuple2('delta', 948),
			_Utils_Tuple2('epsilon', 949),
			_Utils_Tuple2('zeta', 950),
			_Utils_Tuple2('eta', 951),
			_Utils_Tuple2('theta', 952),
			_Utils_Tuple2('iota', 953),
			_Utils_Tuple2('kappa', 954),
			_Utils_Tuple2('lambda', 955),
			_Utils_Tuple2('mu', 956),
			_Utils_Tuple2('nu', 957),
			_Utils_Tuple2('xi', 958),
			_Utils_Tuple2('omicron', 959),
			_Utils_Tuple2('pi', 960),
			_Utils_Tuple2('rho', 961),
			_Utils_Tuple2('sigmaf', 962),
			_Utils_Tuple2('sigma', 963),
			_Utils_Tuple2('tau', 964),
			_Utils_Tuple2('upsilon', 965),
			_Utils_Tuple2('phi', 966),
			_Utils_Tuple2('chi', 967),
			_Utils_Tuple2('psi', 968),
			_Utils_Tuple2('omega', 969),
			_Utils_Tuple2('thetasym', 977),
			_Utils_Tuple2('upsih', 978),
			_Utils_Tuple2('piv', 982),
			_Utils_Tuple2('ensp', 8194),
			_Utils_Tuple2('emsp', 8195),
			_Utils_Tuple2('thinsp', 8201),
			_Utils_Tuple2('zwnj', 8204),
			_Utils_Tuple2('zwj', 8205),
			_Utils_Tuple2('lrm', 8206),
			_Utils_Tuple2('rlm', 8207),
			_Utils_Tuple2('ndash', 8211),
			_Utils_Tuple2('mdash', 8212),
			_Utils_Tuple2('lsquo', 8216),
			_Utils_Tuple2('rsquo', 8217),
			_Utils_Tuple2('sbquo', 8218),
			_Utils_Tuple2('ldquo', 8220),
			_Utils_Tuple2('rdquo', 8221),
			_Utils_Tuple2('bdquo', 8222),
			_Utils_Tuple2('dagger', 8224),
			_Utils_Tuple2('Dagger', 8225),
			_Utils_Tuple2('bull', 8226),
			_Utils_Tuple2('hellip', 8230),
			_Utils_Tuple2('permil', 8240),
			_Utils_Tuple2('prime', 8242),
			_Utils_Tuple2('Prime', 8243),
			_Utils_Tuple2('lsaquo', 8249),
			_Utils_Tuple2('rsaquo', 8250),
			_Utils_Tuple2('oline', 8254),
			_Utils_Tuple2('frasl', 8260),
			_Utils_Tuple2('euro', 8364),
			_Utils_Tuple2('image', 8465),
			_Utils_Tuple2('weierp', 8472),
			_Utils_Tuple2('real', 8476),
			_Utils_Tuple2('trade', 8482),
			_Utils_Tuple2('alefsym', 8501),
			_Utils_Tuple2('larr', 8592),
			_Utils_Tuple2('uarr', 8593),
			_Utils_Tuple2('rarr', 8594),
			_Utils_Tuple2('darr', 8595),
			_Utils_Tuple2('harr', 8596),
			_Utils_Tuple2('crarr', 8629),
			_Utils_Tuple2('lArr', 8656),
			_Utils_Tuple2('uArr', 8657),
			_Utils_Tuple2('rArr', 8658),
			_Utils_Tuple2('dArr', 8659),
			_Utils_Tuple2('hArr', 8660),
			_Utils_Tuple2('forall', 8704),
			_Utils_Tuple2('part', 8706),
			_Utils_Tuple2('exist', 8707),
			_Utils_Tuple2('empty', 8709),
			_Utils_Tuple2('nabla', 8711),
			_Utils_Tuple2('isin', 8712),
			_Utils_Tuple2('notin', 8713),
			_Utils_Tuple2('ni', 8715),
			_Utils_Tuple2('prod', 8719),
			_Utils_Tuple2('sum', 8721),
			_Utils_Tuple2('minus', 8722),
			_Utils_Tuple2('lowast', 8727),
			_Utils_Tuple2('radic', 8730),
			_Utils_Tuple2('prop', 8733),
			_Utils_Tuple2('infin', 8734),
			_Utils_Tuple2('ang', 8736),
			_Utils_Tuple2('and', 8743),
			_Utils_Tuple2('or', 8744),
			_Utils_Tuple2('cap', 8745),
			_Utils_Tuple2('cup', 8746),
			_Utils_Tuple2('int', 8747),
			_Utils_Tuple2('there4', 8756),
			_Utils_Tuple2('sim', 8764),
			_Utils_Tuple2('cong', 8773),
			_Utils_Tuple2('asymp', 8776),
			_Utils_Tuple2('ne', 8800),
			_Utils_Tuple2('equiv', 8801),
			_Utils_Tuple2('le', 8804),
			_Utils_Tuple2('ge', 8805),
			_Utils_Tuple2('sub', 8834),
			_Utils_Tuple2('sup', 8835),
			_Utils_Tuple2('nsub', 8836),
			_Utils_Tuple2('sube', 8838),
			_Utils_Tuple2('supe', 8839),
			_Utils_Tuple2('oplus', 8853),
			_Utils_Tuple2('otimes', 8855),
			_Utils_Tuple2('perp', 8869),
			_Utils_Tuple2('sdot', 8901),
			_Utils_Tuple2('lceil', 8968),
			_Utils_Tuple2('rceil', 8969),
			_Utils_Tuple2('lfloor', 8970),
			_Utils_Tuple2('rfloor', 8971),
			_Utils_Tuple2('lang', 9001),
			_Utils_Tuple2('rang', 9002),
			_Utils_Tuple2('loz', 9674),
			_Utils_Tuple2('spades', 9824),
			_Utils_Tuple2('clubs', 9827),
			_Utils_Tuple2('hearts', 9829),
			_Utils_Tuple2('diams', 9830)
		]));
var $pablohirafuji$elm_markdown$Markdown$Entity$replaceEntity = function (match) {
	return A2(
		$elm$core$Maybe$withDefault,
		match.a2,
		A2(
			$elm$core$Maybe$map,
			A2($elm$core$Basics$composeR, $elm$core$Char$fromCode, $elm$core$String$fromChar),
			A2(
				$elm$core$Maybe$andThen,
				function (a) {
					return A2($elm$core$Dict$get, a, $pablohirafuji$elm_markdown$Markdown$Entity$entities);
				},
				A2(
					$elm$core$Maybe$withDefault,
					$elm$core$Maybe$Nothing,
					$elm$core$List$head(match.eo)))));
};
var $pablohirafuji$elm_markdown$Markdown$Entity$replaceEntities = A2($elm$regex$Regex$replace, $pablohirafuji$elm_markdown$Markdown$Entity$entitiesRegex, $pablohirafuji$elm_markdown$Markdown$Entity$replaceEntity);
var $pablohirafuji$elm_markdown$Markdown$Helpers$escapableRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(\\\\+)([!\"#$%&\\\'()*+,./:;<=>?@[\\\\\\]^_`{|}~-])'));
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $pablohirafuji$elm_markdown$Markdown$Helpers$replaceEscapable = A2(
	$elm$regex$Regex$replace,
	$pablohirafuji$elm_markdown$Markdown$Helpers$escapableRegex,
	function (regexMatch) {
		var _v0 = regexMatch.eo;
		if (((_v0.b && (!_v0.a.$)) && _v0.b.b) && (!_v0.b.a.$)) {
			var backslashes = _v0.a.a;
			var _v1 = _v0.b;
			var escapedStr = _v1.a.a;
			return _Utils_ap(
				A2(
					$elm$core$String$repeat,
					($elm$core$String$length(backslashes) / 2) | 0,
					'\\'),
				escapedStr);
		} else {
			return regexMatch.a2;
		}
	});
var $pablohirafuji$elm_markdown$Markdown$Entity$hexadecimalRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('&#[Xx]([0-9a-fA-F]{1,8});'));
var $elm$core$String$toLower = _String_toLower;
var $pablohirafuji$elm_markdown$Markdown$Entity$hexToInt = A2(
	$elm$core$Basics$composeR,
	$elm$core$String$toLower,
	A2(
		$elm$core$Basics$composeR,
		$elm$core$String$toList,
		A2(
			$elm$core$List$foldl,
			F2(
				function (hexDigit, _int) {
					return ((_int * 16) + A2(
						$elm$core$Basics$modBy,
						39,
						$elm$core$Char$toCode(hexDigit))) - 9;
				}),
			0)));
var $pablohirafuji$elm_markdown$Markdown$Entity$replaceHexadecimal = function (match) {
	return A2(
		$elm$core$Maybe$withDefault,
		match.a2,
		A2(
			$elm$core$Maybe$map,
			A2($elm$core$Basics$composeR, $pablohirafuji$elm_markdown$Markdown$Entity$hexToInt, $pablohirafuji$elm_markdown$Markdown$Entity$validUnicode),
			A2(
				$elm$core$Maybe$withDefault,
				$elm$core$Maybe$Nothing,
				$elm$core$List$head(match.eo))));
};
var $pablohirafuji$elm_markdown$Markdown$Entity$replaceHexadecimals = A2($elm$regex$Regex$replace, $pablohirafuji$elm_markdown$Markdown$Entity$hexadecimalRegex, $pablohirafuji$elm_markdown$Markdown$Entity$replaceHexadecimal);
var $pablohirafuji$elm_markdown$Markdown$Helpers$formatStr = function (str) {
	return $pablohirafuji$elm_markdown$Markdown$Entity$replaceHexadecimals(
		$pablohirafuji$elm_markdown$Markdown$Entity$replaceDecimals(
			$pablohirafuji$elm_markdown$Markdown$Entity$replaceEntities(
				$pablohirafuji$elm_markdown$Markdown$Helpers$replaceEscapable(str))));
};
var $elm$core$String$words = _String_words;
var $pablohirafuji$elm_markdown$Markdown$Block$extractOpenCodeFenceRM = function (match) {
	var _v0 = match.eo;
	if (((_v0.b && _v0.b.b) && (!_v0.b.a.$)) && _v0.b.b.b) {
		var maybeIndent = _v0.a;
		var _v1 = _v0.b;
		var fence = _v1.a.a;
		var _v2 = _v1.b;
		var maybeLanguage = _v2.a;
		return $elm$core$Maybe$Just(
			A2(
				$pablohirafuji$elm_markdown$Markdown$Block$Fenced,
				true,
				{
					cZ: A2($elm$core$String$left, 1, fence),
					c_: $elm$core$String$length(fence),
					u: A2(
						$elm$core$Maybe$withDefault,
						0,
						A2($elm$core$Maybe$map, $elm$core$String$length, maybeIndent)),
					c5: A2(
						$elm$core$Maybe$map,
						$pablohirafuji$elm_markdown$Markdown$Helpers$formatStr,
						A2(
							$elm$core$Maybe$andThen,
							function (lang) {
								return (lang === '') ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(lang);
							},
							$elm$core$List$head(
								A2(
									$elm$core$Maybe$withDefault,
									_List_Nil,
									A2($elm$core$Maybe$map, $elm$core$String$words, maybeLanguage)))))
				}));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $pablohirafuji$elm_markdown$Markdown$Block$openCodeFenceLineRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^( {0,3})(`{3,}(?!.*`)|~{3,}(?!.*~))(.*)$'));
var $pablohirafuji$elm_markdown$Markdown$Block$checkOpenCodeFenceLine = function (_v0) {
	var rawLine = _v0.a;
	var ast = _v0.b;
	return A2(
		$elm$core$Result$fromMaybe,
		_Utils_Tuple2(rawLine, ast),
		A2(
			$elm$core$Maybe$map,
			function (a) {
				return A2($elm$core$List$cons, a, ast);
			},
			A2(
				$elm$core$Maybe$map,
				function (f) {
					return A2($pablohirafuji$elm_markdown$Markdown$Block$CodeBlock, f, '');
				},
				A2(
					$elm$core$Maybe$andThen,
					$pablohirafuji$elm_markdown$Markdown$Block$extractOpenCodeFenceRM,
					$elm$core$List$head(
						A3($elm$regex$Regex$findAtMost, 1, $pablohirafuji$elm_markdown$Markdown$Block$openCodeFenceLineRegex, rawLine))))));
};
var $pablohirafuji$elm_markdown$Markdown$Block$Ordered = function (a) {
	return {$: 1, a: a};
};
var $pablohirafuji$elm_markdown$Markdown$Block$Unordered = {$: 0};
var $pablohirafuji$elm_markdown$Markdown$Block$extractOrderedListRM = function (match) {
	var _v0 = match.eo;
	if (((((((_v0.b && (!_v0.a.$)) && _v0.b.b) && (!_v0.b.a.$)) && _v0.b.b.b) && (!_v0.b.b.a.$)) && _v0.b.b.b.b) && _v0.b.b.b.b.b) {
		var indentString = _v0.a.a;
		var _v1 = _v0.b;
		var start = _v1.a.a;
		var _v2 = _v1.b;
		var delimiter = _v2.a.a;
		var _v3 = _v2.b;
		var maybeIndentSpace = _v3.a;
		var _v4 = _v3.b;
		var maybeRawLine = _v4.a;
		return $elm$core$Maybe$Just(
			_Utils_Tuple3(
				{
					aU: delimiter,
					u: $elm$core$String$length(indentString) + 1,
					W: false,
					bj: A2(
						$elm$core$Maybe$withDefault,
						$pablohirafuji$elm_markdown$Markdown$Block$Unordered,
						A2(
							$elm$core$Maybe$map,
							$pablohirafuji$elm_markdown$Markdown$Block$Ordered,
							$elm$core$String$toInt(start)))
				},
				A2($elm$core$Maybe$withDefault, '', maybeIndentSpace),
				A2($elm$core$Maybe$withDefault, '', maybeRawLine)));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $pablohirafuji$elm_markdown$Markdown$Block$orderedListLineRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^( *(\\d{1,9})([.)])( {0,4}))(?:[ \\t](.*))?$'));
var $pablohirafuji$elm_markdown$Markdown$Block$checkOrderedListLine = function (rawLine) {
	return A2(
		$elm$core$Result$fromMaybe,
		rawLine,
		A2(
			$elm$core$Maybe$andThen,
			$pablohirafuji$elm_markdown$Markdown$Block$extractOrderedListRM,
			$elm$core$List$head(
				A3($elm$regex$Regex$findAtMost, 1, $pablohirafuji$elm_markdown$Markdown$Block$orderedListLineRegex, rawLine))));
};
var $pablohirafuji$elm_markdown$Markdown$Block$extractSetextHeadingRM = function (match) {
	var _v0 = match.eo;
	if (_v0.b && (!_v0.a.$)) {
		var delimiter = _v0.a.a;
		return A2($elm$core$String$startsWith, '=', delimiter) ? $elm$core$Maybe$Just(
			_Utils_Tuple2(1, delimiter)) : $elm$core$Maybe$Just(
			_Utils_Tuple2(2, delimiter));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $pablohirafuji$elm_markdown$Markdown$Block$parseSetextHeadingLine = F3(
	function (rawLine, ast, _v0) {
		var lvl = _v0.a;
		var delimiter = _v0.b;
		if (ast.b && (ast.a.$ === 4)) {
			var _v2 = ast.a;
			var rawText = _v2.a;
			var astTail = ast.b;
			return $elm$core$Maybe$Just(
				A2(
					$elm$core$List$cons,
					A3($pablohirafuji$elm_markdown$Markdown$Block$Heading, rawText, lvl, _List_Nil),
					astTail));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $pablohirafuji$elm_markdown$Markdown$Block$setextHeadingLineRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^ {0,3}(=+|-+)[ \\t]*$'));
var $pablohirafuji$elm_markdown$Markdown$Block$checkSetextHeadingLine = function (_v0) {
	var rawLine = _v0.a;
	var ast = _v0.b;
	return A2(
		$elm$core$Result$fromMaybe,
		_Utils_Tuple2(rawLine, ast),
		A2(
			$elm$core$Maybe$andThen,
			A2($pablohirafuji$elm_markdown$Markdown$Block$parseSetextHeadingLine, rawLine, ast),
			A2(
				$elm$core$Maybe$andThen,
				$pablohirafuji$elm_markdown$Markdown$Block$extractSetextHeadingRM,
				$elm$core$List$head(
					A3($elm$regex$Regex$findAtMost, 1, $pablohirafuji$elm_markdown$Markdown$Block$setextHeadingLineRegex, rawLine)))));
};
var $pablohirafuji$elm_markdown$Markdown$Block$ThematicBreak = {$: 1};
var $pablohirafuji$elm_markdown$Markdown$Block$thematicBreakLineRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^ {0,3}(?:' + ('(?:\\*[ \\t]*){3,}' + ('|(?:_[ \\t]*){3,}' + '|(?:-[ \\t]*){3,})[ \\t]*$'))));
var $pablohirafuji$elm_markdown$Markdown$Block$checkThematicBreakLine = function (_v0) {
	var rawLine = _v0.a;
	var ast = _v0.b;
	return A2(
		$elm$core$Result$fromMaybe,
		_Utils_Tuple2(rawLine, ast),
		A2(
			$elm$core$Maybe$map,
			function (_v1) {
				return A2($elm$core$List$cons, $pablohirafuji$elm_markdown$Markdown$Block$ThematicBreak, ast);
			},
			$elm$core$List$head(
				A3($elm$regex$Regex$findAtMost, 1, $pablohirafuji$elm_markdown$Markdown$Block$thematicBreakLineRegex, rawLine))));
};
var $pablohirafuji$elm_markdown$Markdown$Block$extractUnorderedListRM = function (match) {
	var _v0 = match.eo;
	if ((((((_v0.b && (!_v0.a.$)) && _v0.b.b) && (!_v0.b.a.$)) && _v0.b.b.b) && _v0.b.b.b.b) && (!_v0.b.b.b.b.b)) {
		var indentString = _v0.a.a;
		var _v1 = _v0.b;
		var delimiter = _v1.a.a;
		var _v2 = _v1.b;
		var maybeIndentSpace = _v2.a;
		var _v3 = _v2.b;
		var maybeRawLine = _v3.a;
		return $elm$core$Maybe$Just(
			_Utils_Tuple3(
				{
					aU: delimiter,
					u: $elm$core$String$length(indentString) + 1,
					W: false,
					bj: $pablohirafuji$elm_markdown$Markdown$Block$Unordered
				},
				A2($elm$core$Maybe$withDefault, '', maybeIndentSpace),
				A2($elm$core$Maybe$withDefault, '', maybeRawLine)));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $pablohirafuji$elm_markdown$Markdown$Block$unorderedListLineRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^( *([\\*\\-\\+])( {0,4}))(?:[ \\t](.*))?$'));
var $pablohirafuji$elm_markdown$Markdown$Block$checkUnorderedListLine = function (rawLine) {
	return A2(
		$elm$core$Result$fromMaybe,
		rawLine,
		A2(
			$elm$core$Maybe$andThen,
			$pablohirafuji$elm_markdown$Markdown$Block$extractUnorderedListRM,
			$elm$core$List$head(
				A3($elm$regex$Regex$findAtMost, 1, $pablohirafuji$elm_markdown$Markdown$Block$unorderedListLineRegex, rawLine))));
};
var $pablohirafuji$elm_markdown$Markdown$Block$closeCodeFenceLineRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^ {0,3}(`{3,}|~{3,})\\s*$'));
var $pablohirafuji$elm_markdown$Markdown$Block$isCloseFenceLineHelp = F2(
	function (fence, match) {
		var _v0 = match.eo;
		if (_v0.b && (!_v0.a.$)) {
			var fenceStr = _v0.a.a;
			return (_Utils_cmp(
				$elm$core$String$length(fenceStr),
				fence.c_) > -1) && _Utils_eq(
				A2($elm$core$String$left, 1, fenceStr),
				fence.cZ);
		} else {
			return false;
		}
	});
var $pablohirafuji$elm_markdown$Markdown$Block$isCloseFenceLine = function (fence) {
	return A2(
		$elm$core$Basics$composeR,
		A2($elm$regex$Regex$findAtMost, 1, $pablohirafuji$elm_markdown$Markdown$Block$closeCodeFenceLineRegex),
		A2(
			$elm$core$Basics$composeR,
			$elm$core$List$head,
			A2(
				$elm$core$Basics$composeR,
				$elm$core$Maybe$map(
					$pablohirafuji$elm_markdown$Markdown$Block$isCloseFenceLineHelp(fence)),
				$elm$core$Maybe$withDefault(false))));
};
var $pablohirafuji$elm_markdown$Markdown$Block$continueOrCloseCodeFence = F3(
	function (fence, previousCode, rawLine) {
		return A2($pablohirafuji$elm_markdown$Markdown$Block$isCloseFenceLine, fence, rawLine) ? A2(
			$pablohirafuji$elm_markdown$Markdown$Block$CodeBlock,
			A2($pablohirafuji$elm_markdown$Markdown$Block$Fenced, false, fence),
			previousCode) : A2(
			$pablohirafuji$elm_markdown$Markdown$Block$CodeBlock,
			A2($pablohirafuji$elm_markdown$Markdown$Block$Fenced, true, fence),
			previousCode + (A2($pablohirafuji$elm_markdown$Markdown$Helpers$indentLine, fence.u, rawLine) + '\n'));
	});
var $pablohirafuji$elm_markdown$Markdown$Helpers$ifError = F2(
	function (_function, result) {
		if (!result.$) {
			return result;
		} else {
			var err = result.a;
			return _function(err);
		}
	});
var $pablohirafuji$elm_markdown$Markdown$Helpers$initSpacesRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^ +'));
var $pablohirafuji$elm_markdown$Markdown$Helpers$indentLength = A2(
	$elm$core$Basics$composeR,
	A2(
		$elm$regex$Regex$replace,
		$pablohirafuji$elm_markdown$Markdown$Helpers$tabRegex,
		function (_v0) {
			return '    ';
		}),
	A2(
		$elm$core$Basics$composeR,
		A2($elm$regex$Regex$findAtMost, 1, $pablohirafuji$elm_markdown$Markdown$Helpers$initSpacesRegex),
		A2(
			$elm$core$Basics$composeR,
			$elm$core$List$head,
			A2(
				$elm$core$Basics$composeR,
				$elm$core$Maybe$map(
					A2(
						$elm$core$Basics$composeR,
						function ($) {
							return $.a2;
						},
						$elm$core$String$length)),
				$elm$core$Maybe$withDefault(0)))));
var $pablohirafuji$elm_markdown$Markdown$Block$isBlankLineLast = function (items) {
	isBlankLineLast:
	while (true) {
		if (!items.b) {
			return false;
		} else {
			var item = items.a;
			var itemsTail = items.b;
			_v1$3:
			while (true) {
				if (item.b) {
					switch (item.a.$) {
						case 0:
							if (!item.b.b) {
								return false;
							} else {
								return true;
							}
						case 6:
							var _v2 = item.a;
							var items_ = _v2.b;
							var $temp$items = items_;
							items = $temp$items;
							continue isBlankLineLast;
						default:
							break _v1$3;
					}
				} else {
					break _v1$3;
				}
			}
			return false;
		}
	}
};
var $pablohirafuji$elm_markdown$Markdown$Block$parseTextLine = F2(
	function (rawLine, ast) {
		return A2(
			$elm$core$Maybe$withDefault,
			A2(
				$elm$core$List$cons,
				A2(
					$pablohirafuji$elm_markdown$Markdown$Block$Paragraph,
					$pablohirafuji$elm_markdown$Markdown$Block$formatParagraphLine(rawLine),
					_List_Nil),
				ast),
			A2($pablohirafuji$elm_markdown$Markdown$Block$maybeContinueParagraph, rawLine, ast));
	});
var $pablohirafuji$elm_markdown$Markdown$Block$checkBlockQuote = function (_v16) {
	var rawLine = _v16.a;
	var ast = _v16.b;
	return A2(
		$elm$core$Result$fromMaybe,
		_Utils_Tuple2(rawLine, ast),
		A2(
			$elm$core$Maybe$map,
			$pablohirafuji$elm_markdown$Markdown$Block$parseBlockQuoteLine(ast),
			A2(
				$elm$core$Maybe$map,
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.eo;
					},
					A2(
						$elm$core$Basics$composeR,
						$elm$core$List$head,
						A2(
							$elm$core$Basics$composeR,
							$elm$core$Maybe$withDefault($elm$core$Maybe$Nothing),
							$elm$core$Maybe$withDefault('')))),
				$elm$core$List$head(
					A3($elm$regex$Regex$findAtMost, 1, $pablohirafuji$elm_markdown$Markdown$Block$blockQuoteLineRegex, rawLine)))));
};
var $pablohirafuji$elm_markdown$Markdown$Block$checkListLine = function (_v15) {
	var rawLine = _v15.a;
	var ast = _v15.b;
	return A2(
		$elm$core$Result$mapError,
		function (e) {
			return _Utils_Tuple2(e, ast);
		},
		A2(
			$elm$core$Result$map,
			A2($pablohirafuji$elm_markdown$Markdown$Block$parseListLine, rawLine, ast),
			A2(
				$elm$core$Result$map,
				$pablohirafuji$elm_markdown$Markdown$Block$calcListIndentLength,
				A2(
					$pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
					$pablohirafuji$elm_markdown$Markdown$Block$checkUnorderedListLine,
					$pablohirafuji$elm_markdown$Markdown$Block$checkOrderedListLine(rawLine)))));
};
var $pablohirafuji$elm_markdown$Markdown$Block$incorporateLine = F2(
	function (rawLine, ast) {
		_v11$2:
		while (true) {
			if (ast.b) {
				switch (ast.a.$) {
					case 3:
						if ((ast.a.a.$ === 1) && ast.a.a.a) {
							var _v12 = ast.a;
							var _v13 = _v12.a;
							var fence = _v13.b;
							var code = _v12.b;
							var astTail = ast.b;
							return function (a) {
								return A2($elm$core$List$cons, a, astTail);
							}(
								A3($pablohirafuji$elm_markdown$Markdown$Block$continueOrCloseCodeFence, fence, code, rawLine));
						} else {
							break _v11$2;
						}
					case 6:
						var _v14 = ast.a;
						var model = _v14.a;
						var items = _v14.b;
						var astTail = ast.b;
						return (_Utils_cmp(
							$pablohirafuji$elm_markdown$Markdown$Helpers$indentLength(rawLine),
							model.u) > -1) ? A5($pablohirafuji$elm_markdown$Markdown$Block$parseIndentedListLine, rawLine, model, items, ast, astTail) : A2(
							$elm$core$Result$withDefault,
							A2($pablohirafuji$elm_markdown$Markdown$Block$parseTextLine, rawLine, ast),
							A2(
								$pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
								$pablohirafuji$elm_markdown$Markdown$Block$checkBlockQuote,
								A2(
									$pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
									$pablohirafuji$elm_markdown$Markdown$Block$checkATXHeadingLine,
									A2(
										$pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
										$pablohirafuji$elm_markdown$Markdown$Block$checkSetextHeadingLine,
										A2(
											$pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
											$pablohirafuji$elm_markdown$Markdown$Block$checkOpenCodeFenceLine,
											A2(
												$pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
												$pablohirafuji$elm_markdown$Markdown$Block$checkIndentedCode,
												A2(
													$pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
													$pablohirafuji$elm_markdown$Markdown$Block$checkBlankLine,
													A2(
														$pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
														$pablohirafuji$elm_markdown$Markdown$Block$checkListLine,
														$pablohirafuji$elm_markdown$Markdown$Block$checkThematicBreakLine(
															_Utils_Tuple2(rawLine, ast))))))))));
					default:
						break _v11$2;
				}
			} else {
				break _v11$2;
			}
		}
		return A2($pablohirafuji$elm_markdown$Markdown$Block$parseRawLine, rawLine, ast);
	});
var $pablohirafuji$elm_markdown$Markdown$Block$parseBlockQuoteLine = F2(
	function (ast, rawLine) {
		if (ast.b && (ast.a.$ === 5)) {
			var bqAST = ast.a.a;
			var astTail = ast.b;
			return function (a) {
				return A2($elm$core$List$cons, a, astTail);
			}(
				$pablohirafuji$elm_markdown$Markdown$Block$BlockQuote(
					A2($pablohirafuji$elm_markdown$Markdown$Block$incorporateLine, rawLine, bqAST)));
		} else {
			return function (a) {
				return A2($elm$core$List$cons, a, ast);
			}(
				$pablohirafuji$elm_markdown$Markdown$Block$BlockQuote(
					A2($pablohirafuji$elm_markdown$Markdown$Block$incorporateLine, rawLine, _List_Nil)));
		}
	});
var $pablohirafuji$elm_markdown$Markdown$Block$parseIndentedListLine = F5(
	function (rawLine, model, items, ast, astTail) {
		if (!items.b) {
			return function (a) {
				return A2($elm$core$List$cons, a, astTail);
			}(
				A2(
					$pablohirafuji$elm_markdown$Markdown$Block$List,
					model,
					function (a) {
						return A2($elm$core$List$cons, a, _List_Nil);
					}(
						function (a) {
							return A2($pablohirafuji$elm_markdown$Markdown$Block$incorporateLine, a, _List_Nil);
						}(
							A2($pablohirafuji$elm_markdown$Markdown$Helpers$indentLine, model.u, rawLine)))));
		} else {
			var item = items.a;
			var itemsTail = items.b;
			var indentedRawLine = A2($pablohirafuji$elm_markdown$Markdown$Helpers$indentLine, model.u, rawLine);
			var updateList = function (model_) {
				return function (a) {
					return A2($elm$core$List$cons, a, astTail);
				}(
					A2(
						$pablohirafuji$elm_markdown$Markdown$Block$List,
						model_,
						function (a) {
							return A2($elm$core$List$cons, a, itemsTail);
						}(
							A2($pablohirafuji$elm_markdown$Markdown$Block$incorporateLine, indentedRawLine, item))));
			};
			_v7$3:
			while (true) {
				if (item.b) {
					switch (item.a.$) {
						case 0:
							if (!item.b.b) {
								return updateList(model);
							} else {
								var itemTail = item.b;
								return A2(
									$elm$core$List$all,
									function (block) {
										if (!block.$) {
											return true;
										} else {
											return false;
										}
									},
									itemTail) ? A2($pablohirafuji$elm_markdown$Markdown$Block$parseRawLine, rawLine, ast) : updateList(
									_Utils_update(
										model,
										{W: true}));
							}
						case 6:
							var _v9 = item.a;
							var model_ = _v9.a;
							var items_ = _v9.b;
							var itemTail = item.b;
							return (_Utils_cmp(
								$pablohirafuji$elm_markdown$Markdown$Helpers$indentLength(indentedRawLine),
								model_.u) > -1) ? updateList(model) : ($pablohirafuji$elm_markdown$Markdown$Block$isBlankLineLast(items_) ? updateList(
								_Utils_update(
									model,
									{W: true})) : updateList(model));
						default:
							break _v7$3;
					}
				} else {
					break _v7$3;
				}
			}
			return updateList(model);
		}
	});
var $pablohirafuji$elm_markdown$Markdown$Block$parseListLine = F3(
	function (rawLine, ast, _v0) {
		var listBlock = _v0.a;
		var listRawLine = _v0.b;
		var parsedRawLine = A2($pablohirafuji$elm_markdown$Markdown$Block$incorporateLine, listRawLine, _List_Nil);
		var newList = A2(
			$elm$core$List$cons,
			A2(
				$pablohirafuji$elm_markdown$Markdown$Block$List,
				listBlock,
				_List_fromArray(
					[parsedRawLine])),
			ast);
		_v1$2:
		while (true) {
			if (ast.b) {
				switch (ast.a.$) {
					case 6:
						var _v2 = ast.a;
						var model = _v2.a;
						var items = _v2.b;
						var astTail = ast.b;
						return _Utils_eq(listBlock.aU, model.aU) ? function (a) {
							return A2($elm$core$List$cons, a, astTail);
						}(
							A2(
								$pablohirafuji$elm_markdown$Markdown$Block$List,
								_Utils_update(
									model,
									{
										u: listBlock.u,
										W: model.W || $pablohirafuji$elm_markdown$Markdown$Block$isBlankLineLast(items)
									}),
								A2($elm$core$List$cons, parsedRawLine, items))) : newList;
					case 4:
						var _v3 = ast.a;
						var rawText = _v3.a;
						var inlines = _v3.b;
						var astTail = ast.b;
						if ((parsedRawLine.b && (!parsedRawLine.a.$)) && (!parsedRawLine.b.b)) {
							return A2(
								$elm$core$List$cons,
								A2($pablohirafuji$elm_markdown$Markdown$Block$addToParagraph, rawText, rawLine),
								astTail);
						} else {
							var _v5 = listBlock.bj;
							if (_v5.$ === 1) {
								if (_v5.a === 1) {
									return newList;
								} else {
									var _int = _v5.a;
									return A2(
										$elm$core$List$cons,
										A2($pablohirafuji$elm_markdown$Markdown$Block$addToParagraph, rawText, rawLine),
										astTail);
								}
							} else {
								return newList;
							}
						}
					default:
						break _v1$2;
				}
			} else {
				break _v1$2;
			}
		}
		return newList;
	});
var $pablohirafuji$elm_markdown$Markdown$Block$parseRawLine = F2(
	function (rawLine, ast) {
		return A2(
			$elm$core$Result$withDefault,
			A2($pablohirafuji$elm_markdown$Markdown$Block$parseTextLine, rawLine, ast),
			A2(
				$pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
				$pablohirafuji$elm_markdown$Markdown$Block$checkListLine,
				A2(
					$pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
					$pablohirafuji$elm_markdown$Markdown$Block$checkThematicBreakLine,
					A2(
						$pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
						$pablohirafuji$elm_markdown$Markdown$Block$checkBlockQuote,
						A2(
							$pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
							$pablohirafuji$elm_markdown$Markdown$Block$checkATXHeadingLine,
							A2(
								$pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
								$pablohirafuji$elm_markdown$Markdown$Block$checkSetextHeadingLine,
								A2(
									$pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
									$pablohirafuji$elm_markdown$Markdown$Block$checkOpenCodeFenceLine,
									A2(
										$pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
										$pablohirafuji$elm_markdown$Markdown$Block$checkIndentedCode,
										$pablohirafuji$elm_markdown$Markdown$Block$checkBlankLine(
											_Utils_Tuple2(rawLine, ast))))))))));
	});
var $pablohirafuji$elm_markdown$Markdown$Block$incorporateLines = F2(
	function (rawLines, ast) {
		if (!rawLines.b) {
			return ast;
		} else {
			var rawLine = rawLines.a;
			var rawLinesTail = rawLines.b;
			return A2(
				$pablohirafuji$elm_markdown$Markdown$Block$incorporateLines,
				rawLinesTail,
				A2($pablohirafuji$elm_markdown$Markdown$Block$incorporateLine, rawLine, ast));
		}
	});
var $elm$core$String$lines = _String_lines;
var $pablohirafuji$elm_markdown$Markdown$Block$Custom = F2(
	function (a, b) {
		return {$: 8, a: a, b: b};
	});
var $pablohirafuji$elm_markdown$Markdown$Block$PlainInlines = function (a) {
	return {$: 7, a: a};
};
var $pablohirafuji$elm_markdown$Markdown$Config$Sanitize = function (a) {
	return {$: 1, a: a};
};
var $pablohirafuji$elm_markdown$Markdown$Config$defaultAllowedHtmlAttributes = _List_fromArray(
	['name', 'class']);
var $pablohirafuji$elm_markdown$Markdown$Config$defaultAllowedHtmlElements = _List_fromArray(
	['address', 'article', 'aside', 'b', 'blockquote', 'br', 'caption', 'center', 'cite', 'code', 'col', 'colgroup', 'dd', 'details', 'div', 'dl', 'dt', 'figcaption', 'figure', 'footer', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6', 'hr', 'i', 'legend', 'li', 'menu', 'menuitem', 'nav', 'ol', 'optgroup', 'option', 'p', 'pre', 'section', 'strike', 'summary', 'small', 'table', 'tbody', 'td', 'tfoot', 'th', 'thead', 'tr', 'ul']);
var $pablohirafuji$elm_markdown$Markdown$Config$defaultSanitizeOptions = {dq: $pablohirafuji$elm_markdown$Markdown$Config$defaultAllowedHtmlAttributes, dr: $pablohirafuji$elm_markdown$Markdown$Config$defaultAllowedHtmlElements};
var $pablohirafuji$elm_markdown$Markdown$Config$defaultOptions = {
	ea: $pablohirafuji$elm_markdown$Markdown$Config$Sanitize($pablohirafuji$elm_markdown$Markdown$Config$defaultSanitizeOptions),
	el: false
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$initParser = F3(
	function (options, refs, rawText) {
		return {b: _List_Nil, e6: options, B: rawText, dg: refs, j: _List_Nil};
	});
var $pablohirafuji$elm_markdown$Markdown$Inline$CodeInline = function (a) {
	return {$: 2, a: a};
};
var $pablohirafuji$elm_markdown$Markdown$Inline$Emphasis = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var $pablohirafuji$elm_markdown$Markdown$Inline$HardLineBreak = {$: 1};
var $pablohirafuji$elm_markdown$Markdown$Inline$HtmlInline = F3(
	function (a, b, c) {
		return {$: 5, a: a, b: b, c: c};
	});
var $pablohirafuji$elm_markdown$Markdown$Inline$Image = F3(
	function (a, b, c) {
		return {$: 4, a: a, b: b, c: c};
	});
var $pablohirafuji$elm_markdown$Markdown$Inline$Link = F3(
	function (a, b, c) {
		return {$: 3, a: a, b: b, c: c};
	});
var $pablohirafuji$elm_markdown$Markdown$Inline$Text = function (a) {
	return {$: 0, a: a};
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$matchToInline = function (_v0) {
	var match = _v0;
	var _v1 = match.bj;
	switch (_v1.$) {
		case 0:
			return $pablohirafuji$elm_markdown$Markdown$Inline$Text(match.t);
		case 1:
			return $pablohirafuji$elm_markdown$Markdown$Inline$HardLineBreak;
		case 2:
			return $pablohirafuji$elm_markdown$Markdown$Inline$CodeInline(match.t);
		case 3:
			var _v2 = _v1.a;
			var text = _v2.a;
			var url = _v2.b;
			return A3(
				$pablohirafuji$elm_markdown$Markdown$Inline$Link,
				url,
				$elm$core$Maybe$Nothing,
				_List_fromArray(
					[
						$pablohirafuji$elm_markdown$Markdown$Inline$Text(text)
					]));
		case 4:
			var _v3 = _v1.a;
			var url = _v3.a;
			var maybeTitle = _v3.b;
			return A3(
				$pablohirafuji$elm_markdown$Markdown$Inline$Link,
				url,
				maybeTitle,
				$pablohirafuji$elm_markdown$Markdown$InlineParser$matchesToInlines(match.b));
		case 5:
			var _v4 = _v1.a;
			var url = _v4.a;
			var maybeTitle = _v4.b;
			return A3(
				$pablohirafuji$elm_markdown$Markdown$Inline$Image,
				url,
				maybeTitle,
				$pablohirafuji$elm_markdown$Markdown$InlineParser$matchesToInlines(match.b));
		case 6:
			var model = _v1.a;
			return A3(
				$pablohirafuji$elm_markdown$Markdown$Inline$HtmlInline,
				model.be,
				model.bC,
				$pablohirafuji$elm_markdown$Markdown$InlineParser$matchesToInlines(match.b));
		default:
			var length = _v1.a;
			return A2(
				$pablohirafuji$elm_markdown$Markdown$Inline$Emphasis,
				length,
				$pablohirafuji$elm_markdown$Markdown$InlineParser$matchesToInlines(match.b));
	}
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$matchesToInlines = function (matches) {
	return A2($elm$core$List$map, $pablohirafuji$elm_markdown$Markdown$InlineParser$matchToInline, matches);
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$Match = $elm$core$Basics$identity;
var $pablohirafuji$elm_markdown$Markdown$InlineParser$prepareChildMatch = F2(
	function (parentMatch, childMatch) {
		return _Utils_update(
			childMatch,
			{dE: childMatch.dE - parentMatch.N, di: childMatch.di - parentMatch.N, aI: childMatch.aI - parentMatch.N, N: childMatch.N - parentMatch.N});
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$addChild = F2(
	function (parentMatch, childMatch) {
		return _Utils_update(
			parentMatch,
			{
				b: A2(
					$elm$core$List$cons,
					A2($pablohirafuji$elm_markdown$Markdown$InlineParser$prepareChildMatch, parentMatch, childMatch),
					parentMatch.b)
			});
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$organizeMatch = F2(
	function (_v0, matches) {
		var match = _v0;
		if (!matches.b) {
			return _List_fromArray(
				[match]);
		} else {
			var prevMatch = matches.a;
			var matchesTail = matches.b;
			return (_Utils_cmp(prevMatch.dE, match.di) < 1) ? A2($elm$core$List$cons, match, matches) : (((_Utils_cmp(prevMatch.di, match.di) < 0) && (_Utils_cmp(prevMatch.dE, match.dE) > 0)) ? A2(
				$elm$core$List$cons,
				A2($pablohirafuji$elm_markdown$Markdown$InlineParser$addChild, prevMatch, match),
				matchesTail) : matches);
		}
	});
var $elm$core$List$sortBy = _List_sortBy;
function $pablohirafuji$elm_markdown$Markdown$InlineParser$cyclic$organizeMatches() {
	return A2(
		$elm$core$Basics$composeR,
		$elm$core$List$sortBy(
			function (_v0) {
				var match = _v0;
				return match.di;
			}),
		A2(
			$elm$core$Basics$composeR,
			A2($elm$core$List$foldl, $pablohirafuji$elm_markdown$Markdown$InlineParser$organizeMatch, _List_Nil),
			$elm$core$List$map(
				function (_v1) {
					var match = _v1;
					return _Utils_update(
						match,
						{
							b: $pablohirafuji$elm_markdown$Markdown$InlineParser$cyclic$organizeMatches()(match.b)
						});
				})));
}
var $pablohirafuji$elm_markdown$Markdown$InlineParser$organizeMatches = $pablohirafuji$elm_markdown$Markdown$InlineParser$cyclic$organizeMatches();
$pablohirafuji$elm_markdown$Markdown$InlineParser$cyclic$organizeMatches = function () {
	return $pablohirafuji$elm_markdown$Markdown$InlineParser$organizeMatches;
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$organizeParserMatches = function (model) {
	return _Utils_update(
		model,
		{
			b: $pablohirafuji$elm_markdown$Markdown$InlineParser$organizeMatches(model.b)
		});
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$NormalType = {$: 0};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$normalMatch = function (text) {
	return {
		dE: 0,
		b: _List_Nil,
		di: 0,
		t: $pablohirafuji$elm_markdown$Markdown$Helpers$formatStr(text),
		aI: 0,
		N: 0,
		bj: $pablohirafuji$elm_markdown$Markdown$InlineParser$NormalType
	};
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$parseTextMatch = F3(
	function (rawText, _v2, parsedMatches) {
		var matchModel = _v2;
		var updtMatch = _Utils_update(
			matchModel,
			{
				b: A3($pablohirafuji$elm_markdown$Markdown$InlineParser$parseTextMatches, matchModel.t, _List_Nil, matchModel.b)
			});
		if (!parsedMatches.b) {
			var finalStr = A2($elm$core$String$dropLeft, matchModel.dE, rawText);
			return $elm$core$String$isEmpty(finalStr) ? _List_fromArray(
				[updtMatch]) : _List_fromArray(
				[
					updtMatch,
					$pablohirafuji$elm_markdown$Markdown$InlineParser$normalMatch(finalStr)
				]);
		} else {
			var matchHead = parsedMatches.a;
			var matchesTail = parsedMatches.b;
			return _Utils_eq(matchHead.bj, $pablohirafuji$elm_markdown$Markdown$InlineParser$NormalType) ? A2($elm$core$List$cons, updtMatch, parsedMatches) : (_Utils_eq(matchModel.dE, matchHead.di) ? A2($elm$core$List$cons, updtMatch, parsedMatches) : ((_Utils_cmp(matchModel.dE, matchHead.di) < 0) ? A2(
				$elm$core$List$cons,
				updtMatch,
				A2(
					$elm$core$List$cons,
					$pablohirafuji$elm_markdown$Markdown$InlineParser$normalMatch(
						A3($elm$core$String$slice, matchModel.dE, matchHead.di, rawText)),
					parsedMatches)) : parsedMatches));
		}
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$parseTextMatches = F3(
	function (rawText, parsedMatches, matches) {
		parseTextMatches:
		while (true) {
			if (!matches.b) {
				if (!parsedMatches.b) {
					return $elm$core$String$isEmpty(rawText) ? _List_Nil : _List_fromArray(
						[
							$pablohirafuji$elm_markdown$Markdown$InlineParser$normalMatch(rawText)
						]);
				} else {
					var matchModel = parsedMatches.a;
					return (matchModel.di > 0) ? A2(
						$elm$core$List$cons,
						$pablohirafuji$elm_markdown$Markdown$InlineParser$normalMatch(
							A2($elm$core$String$left, matchModel.di, rawText)),
						parsedMatches) : parsedMatches;
				}
			} else {
				var match = matches.a;
				var matchesTail = matches.b;
				var $temp$rawText = rawText,
					$temp$parsedMatches = A3($pablohirafuji$elm_markdown$Markdown$InlineParser$parseTextMatch, rawText, match, parsedMatches),
					$temp$matches = matchesTail;
				rawText = $temp$rawText;
				parsedMatches = $temp$parsedMatches;
				matches = $temp$matches;
				continue parseTextMatches;
			}
		}
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$parseText = function (model) {
	return _Utils_update(
		model,
		{
			b: A3($pablohirafuji$elm_markdown$Markdown$InlineParser$parseTextMatches, model.B, _List_Nil, model.b)
		});
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$angleBracketLTokenRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(\\\\*)(\\<)'));
var $elm$regex$Regex$find = _Regex_findAtMost(_Regex_infinity);
var $pablohirafuji$elm_markdown$Markdown$InlineParser$CharToken = function (a) {
	return {$: 3, a: a};
};
var $pablohirafuji$elm_markdown$Markdown$Helpers$isEven = function (_int) {
	return !A2($elm$core$Basics$modBy, 2, _int);
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToAngleBracketLToken = function (regMatch) {
	var _v0 = regMatch.eo;
	if ((_v0.b && _v0.b.b) && (!_v0.b.a.$)) {
		var maybeBackslashes = _v0.a;
		var _v1 = _v0.b;
		var delimiter = _v1.a.a;
		var backslashesLength = A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($elm$core$Maybe$map, $elm$core$String$length, maybeBackslashes));
		return $pablohirafuji$elm_markdown$Markdown$Helpers$isEven(backslashesLength) ? $elm$core$Maybe$Just(
			{
				eY: regMatch.eY + backslashesLength,
				c: 1,
				g: $pablohirafuji$elm_markdown$Markdown$InlineParser$CharToken('<')
			}) : $elm$core$Maybe$Nothing;
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$findAngleBracketLTokens = function (str) {
	return A2(
		$elm$core$List$filterMap,
		$pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToAngleBracketLToken,
		A2($elm$regex$Regex$find, $pablohirafuji$elm_markdown$Markdown$InlineParser$angleBracketLTokenRegex, str));
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$angleBracketRTokenRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(\\\\*)(\\>)'));
var $pablohirafuji$elm_markdown$Markdown$InlineParser$RightAngleBracket = function (a) {
	return {$: 4, a: a};
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToAngleBracketRToken = function (regMatch) {
	var _v0 = regMatch.eo;
	if ((_v0.b && _v0.b.b) && (!_v0.b.a.$)) {
		var maybeBackslashes = _v0.a;
		var _v1 = _v0.b;
		var backslashesLength = A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($elm$core$Maybe$map, $elm$core$String$length, maybeBackslashes));
		return $elm$core$Maybe$Just(
			{
				eY: regMatch.eY + backslashesLength,
				c: 1,
				g: $pablohirafuji$elm_markdown$Markdown$InlineParser$RightAngleBracket(
					!$pablohirafuji$elm_markdown$Markdown$Helpers$isEven(backslashesLength))
			});
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$findAngleBracketRTokens = function (str) {
	return A2(
		$elm$core$List$filterMap,
		$pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToAngleBracketRToken,
		A2($elm$regex$Regex$find, $pablohirafuji$elm_markdown$Markdown$InlineParser$angleBracketRTokenRegex, str));
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$asteriskEmphasisTokenRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(\\\\*)([^*])?(\\*+)([^*])?'));
var $pablohirafuji$elm_markdown$Markdown$InlineParser$EmphasisToken = F2(
	function (a, b) {
		return {$: 6, a: a, b: b};
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$punctuationRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('[!-#%-\\*,-/:;\\?@\\[-\\]_\\{\\}]'));
var $pablohirafuji$elm_markdown$Markdown$InlineParser$containPunctuation = $elm$regex$Regex$contains($pablohirafuji$elm_markdown$Markdown$InlineParser$punctuationRegex);
var $pablohirafuji$elm_markdown$Markdown$InlineParser$spaceRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('\\s'));
var $pablohirafuji$elm_markdown$Markdown$InlineParser$containSpace = $elm$regex$Regex$contains($pablohirafuji$elm_markdown$Markdown$InlineParser$spaceRegex);
var $pablohirafuji$elm_markdown$Markdown$InlineParser$charFringeRank = function (_char) {
	var string = $elm$core$String$fromChar(_char);
	return $pablohirafuji$elm_markdown$Markdown$InlineParser$containSpace(string) ? 0 : ($pablohirafuji$elm_markdown$Markdown$InlineParser$containPunctuation(string) ? 1 : 2);
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$maybeCharFringeRank = function (maybeChar) {
	return A2(
		$elm$core$Maybe$withDefault,
		0,
		A2($elm$core$Maybe$map, $pablohirafuji$elm_markdown$Markdown$InlineParser$charFringeRank, maybeChar));
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$getFringeRank = A2(
	$elm$core$Basics$composeR,
	$elm$core$Maybe$map(
		A2(
			$elm$core$Basics$composeR,
			$elm$core$String$uncons,
			A2(
				$elm$core$Basics$composeR,
				$elm$core$Maybe$map($elm$core$Tuple$first),
				$pablohirafuji$elm_markdown$Markdown$InlineParser$maybeCharFringeRank))),
	$elm$core$Maybe$withDefault(0));
var $elm$core$Basics$neq = _Utils_notEqual;
var $pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToEmphasisToken = F3(
	function (_char, rawText, regMatch) {
		var _v0 = regMatch.eo;
		if ((((_v0.b && _v0.b.b) && _v0.b.b.b) && (!_v0.b.b.a.$)) && _v0.b.b.b.b) {
			var maybeBackslashes = _v0.a;
			var _v1 = _v0.b;
			var maybeLeftFringe = _v1.a;
			var _v2 = _v1.b;
			var delimiter = _v2.a.a;
			var _v3 = _v2.b;
			var maybeRightFringe = _v3.a;
			var leftFringeLength = A2(
				$elm$core$Maybe$withDefault,
				0,
				A2($elm$core$Maybe$map, $elm$core$String$length, maybeLeftFringe));
			var mLeftFringe = ((!(!regMatch.eY)) && (!leftFringeLength)) ? $elm$core$Maybe$Just(
				A3($elm$core$String$slice, regMatch.eY - 1, regMatch.eY, rawText)) : maybeLeftFringe;
			var backslashesLength = A2(
				$elm$core$Maybe$withDefault,
				0,
				A2($elm$core$Maybe$map, $elm$core$String$length, maybeBackslashes));
			var isEscaped = ((!$pablohirafuji$elm_markdown$Markdown$Helpers$isEven(backslashesLength)) && (!leftFringeLength)) || _Utils_eq(
				mLeftFringe,
				$elm$core$Maybe$Just('\\'));
			var delimiterLength = isEscaped ? ($elm$core$String$length(delimiter) - 1) : $elm$core$String$length(delimiter);
			var fringeRank = _Utils_Tuple2(
				isEscaped ? 1 : $pablohirafuji$elm_markdown$Markdown$InlineParser$getFringeRank(mLeftFringe),
				$pablohirafuji$elm_markdown$Markdown$InlineParser$getFringeRank(maybeRightFringe));
			var index = ((regMatch.eY + backslashesLength) + leftFringeLength) + (isEscaped ? 1 : 0);
			return ((delimiterLength <= 0) || ((_char === '_') && _Utils_eq(
				fringeRank,
				_Utils_Tuple2(2, 2)))) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(
				{
					eY: index,
					c: delimiterLength,
					g: A2($pablohirafuji$elm_markdown$Markdown$InlineParser$EmphasisToken, _char, fringeRank)
				});
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$findAsteriskEmphasisTokens = function (str) {
	return A2(
		$elm$core$List$filterMap,
		A2($pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToEmphasisToken, '*', str),
		A2($elm$regex$Regex$find, $pablohirafuji$elm_markdown$Markdown$InlineParser$asteriskEmphasisTokenRegex, str));
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$codeTokenRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(\\\\*)(\\`+)'));
var $pablohirafuji$elm_markdown$Markdown$InlineParser$CodeToken = function (a) {
	return {$: 0, a: a};
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToCodeToken = function (regMatch) {
	var _v0 = regMatch.eo;
	if ((_v0.b && _v0.b.b) && (!_v0.b.a.$)) {
		var maybeBackslashes = _v0.a;
		var _v1 = _v0.b;
		var backtick = _v1.a.a;
		var backslashesLength = A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($elm$core$Maybe$map, $elm$core$String$length, maybeBackslashes));
		return $elm$core$Maybe$Just(
			{
				eY: regMatch.eY + backslashesLength,
				c: $elm$core$String$length(backtick),
				g: $pablohirafuji$elm_markdown$Markdown$InlineParser$CodeToken(
					!$pablohirafuji$elm_markdown$Markdown$Helpers$isEven(backslashesLength))
			});
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$findCodeTokens = function (str) {
	return A2(
		$elm$core$List$filterMap,
		$pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToCodeToken,
		A2($elm$regex$Regex$find, $pablohirafuji$elm_markdown$Markdown$InlineParser$codeTokenRegex, str));
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$hardBreakTokenRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(?:(\\\\+)|( {2,}))\\n'));
var $pablohirafuji$elm_markdown$Markdown$InlineParser$HardLineBreakToken = {$: 8};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToHardBreakToken = function (regMatch) {
	var _v0 = regMatch.eo;
	_v0$2:
	while (true) {
		if (_v0.b) {
			if (!_v0.a.$) {
				var backslashes = _v0.a.a;
				var backslashesLength = $elm$core$String$length(backslashes);
				return (!$pablohirafuji$elm_markdown$Markdown$Helpers$isEven(backslashesLength)) ? $elm$core$Maybe$Just(
					{eY: (regMatch.eY + backslashesLength) - 1, c: 2, g: $pablohirafuji$elm_markdown$Markdown$InlineParser$HardLineBreakToken}) : $elm$core$Maybe$Nothing;
			} else {
				if (_v0.b.b && (!_v0.b.a.$)) {
					var _v1 = _v0.b;
					return $elm$core$Maybe$Just(
						{
							eY: regMatch.eY,
							c: $elm$core$String$length(regMatch.a2),
							g: $pablohirafuji$elm_markdown$Markdown$InlineParser$HardLineBreakToken
						});
				} else {
					break _v0$2;
				}
			}
		} else {
			break _v0$2;
		}
	}
	return $elm$core$Maybe$Nothing;
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToSoftHardBreakToken = function (regMatch) {
	var _v0 = regMatch.eo;
	_v0$2:
	while (true) {
		if (_v0.b) {
			if (!_v0.a.$) {
				var backslashes = _v0.a.a;
				var backslashesLength = $elm$core$String$length(backslashes);
				return $pablohirafuji$elm_markdown$Markdown$Helpers$isEven(backslashesLength) ? $elm$core$Maybe$Just(
					{eY: regMatch.eY + backslashesLength, c: 1, g: $pablohirafuji$elm_markdown$Markdown$InlineParser$HardLineBreakToken}) : $elm$core$Maybe$Just(
					{eY: (regMatch.eY + backslashesLength) - 1, c: 2, g: $pablohirafuji$elm_markdown$Markdown$InlineParser$HardLineBreakToken});
			} else {
				if (_v0.b.b) {
					var _v1 = _v0.b;
					var maybeSpaces = _v1.a;
					return $elm$core$Maybe$Just(
						{
							eY: regMatch.eY,
							c: $elm$core$String$length(regMatch.a2),
							g: $pablohirafuji$elm_markdown$Markdown$InlineParser$HardLineBreakToken
						});
				} else {
					break _v0$2;
				}
			}
		} else {
			break _v0$2;
		}
	}
	return $elm$core$Maybe$Nothing;
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$softAsHardLineBreakTokenRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(?:(\\\\+)|( *))\\n'));
var $pablohirafuji$elm_markdown$Markdown$InlineParser$findHardBreakTokens = F2(
	function (softAsHardLineBreak, str) {
		return softAsHardLineBreak ? A2(
			$elm$core$List$filterMap,
			$pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToSoftHardBreakToken,
			A2($elm$regex$Regex$find, $pablohirafuji$elm_markdown$Markdown$InlineParser$softAsHardLineBreakTokenRegex, str)) : A2(
			$elm$core$List$filterMap,
			$pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToHardBreakToken,
			A2($elm$regex$Regex$find, $pablohirafuji$elm_markdown$Markdown$InlineParser$hardBreakTokenRegex, str));
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$linkImageCloseTokenRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(\\\\*)(\\])'));
var $pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToLinkImageCloseToken = function (regMatch) {
	var _v0 = regMatch.eo;
	if ((_v0.b && _v0.b.b) && (!_v0.b.a.$)) {
		var maybeBackslashes = _v0.a;
		var _v1 = _v0.b;
		var delimiter = _v1.a.a;
		var backslashesLength = A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($elm$core$Maybe$map, $elm$core$String$length, maybeBackslashes));
		return $pablohirafuji$elm_markdown$Markdown$Helpers$isEven(backslashesLength) ? $elm$core$Maybe$Just(
			{
				eY: regMatch.eY + backslashesLength,
				c: 1,
				g: $pablohirafuji$elm_markdown$Markdown$InlineParser$CharToken(']')
			}) : $elm$core$Maybe$Nothing;
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$findLinkImageCloseTokens = function (str) {
	return A2(
		$elm$core$List$filterMap,
		$pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToLinkImageCloseToken,
		A2($elm$regex$Regex$find, $pablohirafuji$elm_markdown$Markdown$InlineParser$linkImageCloseTokenRegex, str));
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$linkImageOpenTokenRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(\\\\*)(\\!)?(\\[)'));
var $pablohirafuji$elm_markdown$Markdown$InlineParser$ImageOpenToken = {$: 2};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$LinkOpenToken = function (a) {
	return {$: 1, a: a};
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToLinkImageOpenToken = function (regMatch) {
	var _v0 = regMatch.eo;
	if (((_v0.b && _v0.b.b) && _v0.b.b.b) && (!_v0.b.b.a.$)) {
		var maybeBackslashes = _v0.a;
		var _v1 = _v0.b;
		var maybeImageOpen = _v1.a;
		var _v2 = _v1.b;
		var delimiter = _v2.a.a;
		var backslashesLength = A2(
			$elm$core$Maybe$withDefault,
			0,
			A2($elm$core$Maybe$map, $elm$core$String$length, maybeBackslashes));
		var isEscaped = !$pablohirafuji$elm_markdown$Markdown$Helpers$isEven(backslashesLength);
		var index = (regMatch.eY + backslashesLength) + ((isEscaped && _Utils_eq(
			maybeImageOpen,
			$elm$core$Maybe$Just('!'))) ? 1 : 0);
		var meaning = isEscaped ? A2(
			$elm$core$Maybe$map,
			function (_v3) {
				return $pablohirafuji$elm_markdown$Markdown$InlineParser$LinkOpenToken(true);
			},
			maybeImageOpen) : $elm$core$Maybe$Just(
			A2(
				$elm$core$Maybe$withDefault,
				$pablohirafuji$elm_markdown$Markdown$InlineParser$LinkOpenToken(true),
				A2(
					$elm$core$Maybe$map,
					function (_v4) {
						return $pablohirafuji$elm_markdown$Markdown$InlineParser$ImageOpenToken;
					},
					maybeImageOpen)));
		var length = _Utils_eq(
			meaning,
			$elm$core$Maybe$Just($pablohirafuji$elm_markdown$Markdown$InlineParser$ImageOpenToken)) ? 2 : 1;
		var toModel = function (m) {
			return {eY: index, c: length, g: m};
		};
		return A2($elm$core$Maybe$map, toModel, meaning);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$findLinkImageOpenTokens = function (str) {
	return A2(
		$elm$core$List$filterMap,
		$pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToLinkImageOpenToken,
		A2($elm$regex$Regex$find, $pablohirafuji$elm_markdown$Markdown$InlineParser$linkImageOpenTokenRegex, str));
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$underlineEmphasisTokenRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('(\\\\*)([^_])?(\\_+)([^_])?'));
var $pablohirafuji$elm_markdown$Markdown$InlineParser$findUnderlineEmphasisTokens = function (str) {
	return A2(
		$elm$core$List$filterMap,
		A2($pablohirafuji$elm_markdown$Markdown$InlineParser$regMatchToEmphasisToken, '_', str),
		A2($elm$regex$Regex$find, $pablohirafuji$elm_markdown$Markdown$InlineParser$underlineEmphasisTokenRegex, str));
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$tokenize = function (model) {
	return _Utils_update(
		model,
		{
			j: A2(
				$elm$core$List$sortBy,
				function ($) {
					return $.eY;
				},
				_Utils_ap(
					$pablohirafuji$elm_markdown$Markdown$InlineParser$findAngleBracketRTokens(model.B),
					_Utils_ap(
						$pablohirafuji$elm_markdown$Markdown$InlineParser$findAngleBracketLTokens(model.B),
						_Utils_ap(
							A2($pablohirafuji$elm_markdown$Markdown$InlineParser$findHardBreakTokens, model.e6.el, model.B),
							_Utils_ap(
								$pablohirafuji$elm_markdown$Markdown$InlineParser$findLinkImageCloseTokens(model.B),
								_Utils_ap(
									$pablohirafuji$elm_markdown$Markdown$InlineParser$findLinkImageOpenTokens(model.B),
									_Utils_ap(
										$pablohirafuji$elm_markdown$Markdown$InlineParser$findUnderlineEmphasisTokens(model.B),
										_Utils_ap(
											$pablohirafuji$elm_markdown$Markdown$InlineParser$findAsteriskEmphasisTokens(model.B),
											$pablohirafuji$elm_markdown$Markdown$InlineParser$findCodeTokens(model.B)))))))))
		});
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$CodeType = {$: 2};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$EmphasisType = function (a) {
	return {$: 7, a: a};
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$HtmlType = function (a) {
	return {$: 6, a: a};
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$ImageType = function (a) {
	return {$: 5, a: a};
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$LinkType = function (a) {
	return {$: 4, a: a};
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$addMatch = F2(
	function (model, match) {
		return _Utils_update(
			model,
			{
				b: A2($elm$core$List$cons, match, model.b)
			});
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$addToken = F2(
	function (model, token) {
		return _Utils_update(
			model,
			{
				j: A2($elm$core$List$cons, token, model.j)
			});
	});
var $elm$core$Result$andThen = F2(
	function (callback, result) {
		if (!result.$) {
			var value = result.a;
			return callback(value);
		} else {
			var msg = result.a;
			return $elm$core$Result$Err(msg);
		}
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$applyTTM = F2(
	function (finderFunction, model) {
		return finderFunction(
			_Utils_Tuple2(
				model.j,
				_Utils_update(
					model,
					{j: _List_Nil})));
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$AutolinkType = function (a) {
	return {$: 3, a: a};
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$decodeUrlRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('%(?:3B|2C|2F|3F|3A|40|26|3D|2B|24|23|25)'));
var $elm$url$Url$percentEncode = _Url_percentEncode;
var $pablohirafuji$elm_markdown$Markdown$InlineParser$encodeUrl = A2(
	$elm$core$Basics$composeR,
	$elm$url$Url$percentEncode,
	A2(
		$elm$regex$Regex$replace,
		$pablohirafuji$elm_markdown$Markdown$InlineParser$decodeUrlRegex,
		function (match) {
			return A2(
				$elm$core$Maybe$withDefault,
				match.a2,
				$elm$url$Url$percentDecode(match.a2));
		}));
var $pablohirafuji$elm_markdown$Markdown$InlineParser$urlRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^([A-Za-z][A-Za-z0-9.+\\-]{1,31}:[^<>\\x00-\\x20]*)$'));
var $pablohirafuji$elm_markdown$Markdown$InlineParser$autolinkToMatch = function (_v0) {
	var match = _v0;
	return A2($elm$regex$Regex$contains, $pablohirafuji$elm_markdown$Markdown$InlineParser$urlRegex, match.t) ? $elm$core$Result$Ok(
		_Utils_update(
			match,
			{
				bj: $pablohirafuji$elm_markdown$Markdown$InlineParser$AutolinkType(
					_Utils_Tuple2(
						match.t,
						$pablohirafuji$elm_markdown$Markdown$InlineParser$encodeUrl(match.t)))
			})) : $elm$core$Result$Err(match);
};
var $pablohirafuji$elm_markdown$Markdown$Helpers$whiteSpaceChars = ' \\t\\f\\v\\r\\n';
var $pablohirafuji$elm_markdown$Markdown$InlineParser$hrefRegex = '(?:<([^<>' + ($pablohirafuji$elm_markdown$Markdown$Helpers$whiteSpaceChars + (']*)>|([^' + ($pablohirafuji$elm_markdown$Markdown$Helpers$whiteSpaceChars + ('\\(\\)\\\\]*(?:\\\\.[^' + ($pablohirafuji$elm_markdown$Markdown$Helpers$whiteSpaceChars + '\\(\\)\\\\]*)*))')))));
var $pablohirafuji$elm_markdown$Markdown$Helpers$titleRegex = '(?:[' + ($pablohirafuji$elm_markdown$Markdown$Helpers$whiteSpaceChars + (']+' + ('(?:\'([^\'\\\\]*(?:\\\\.[^\'\\\\]*)*)\'|' + ('\"([^\"\\\\]*(?:\\\\.[^\"\\\\]*)*)\"|' + '\\(([^\\)\\\\]*(?:\\\\.[^\\)\\\\]*)*)\\)))?'))));
var $pablohirafuji$elm_markdown$Markdown$InlineParser$inlineLinkTypeOrImageTypeRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^\\(\\s*' + ($pablohirafuji$elm_markdown$Markdown$InlineParser$hrefRegex + ($pablohirafuji$elm_markdown$Markdown$Helpers$titleRegex + '\\s*\\)'))));
var $pablohirafuji$elm_markdown$Markdown$InlineParser$prepareUrlAndTitle = function (_v0) {
	var rawUrl = _v0.a;
	var maybeTitle = _v0.b;
	return _Utils_Tuple2(
		$pablohirafuji$elm_markdown$Markdown$InlineParser$encodeUrl(
			$pablohirafuji$elm_markdown$Markdown$Helpers$formatStr(rawUrl)),
		A2($elm$core$Maybe$map, $pablohirafuji$elm_markdown$Markdown$Helpers$formatStr, maybeTitle));
};
var $pablohirafuji$elm_markdown$Markdown$Helpers$returnFirstJust = function (maybes) {
	var process = F2(
		function (a, maybeFound) {
			if (!maybeFound.$) {
				var found = maybeFound.a;
				return $elm$core$Maybe$Just(found);
			} else {
				return a;
			}
		});
	return A3($elm$core$List$foldl, process, $elm$core$Maybe$Nothing, maybes);
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$inlineLinkTypeOrImageTypeRegexToMatch = F3(
	function (matchModel, model, regexMatch) {
		var _v0 = regexMatch.eo;
		if ((((_v0.b && _v0.b.b) && _v0.b.b.b) && _v0.b.b.b.b) && _v0.b.b.b.b.b) {
			var maybeRawUrlAngleBrackets = _v0.a;
			var _v1 = _v0.b;
			var maybeRawUrlWithoutBrackets = _v1.a;
			var _v2 = _v1.b;
			var maybeTitleSingleQuotes = _v2.a;
			var _v3 = _v2.b;
			var maybeTitleDoubleQuotes = _v3.a;
			var _v4 = _v3.b;
			var maybeTitleParenthesis = _v4.a;
			var maybeTitle = $pablohirafuji$elm_markdown$Markdown$Helpers$returnFirstJust(
				_List_fromArray(
					[maybeTitleSingleQuotes, maybeTitleDoubleQuotes, maybeTitleParenthesis]));
			var toMatch = function (rawUrl) {
				return _Utils_update(
					matchModel,
					{
						dE: matchModel.dE + $elm$core$String$length(regexMatch.a2),
						bj: function () {
							var _v5 = matchModel.bj;
							if (_v5.$ === 5) {
								return $pablohirafuji$elm_markdown$Markdown$InlineParser$ImageType;
							} else {
								return $pablohirafuji$elm_markdown$Markdown$InlineParser$LinkType;
							}
						}()(
							$pablohirafuji$elm_markdown$Markdown$InlineParser$prepareUrlAndTitle(
								_Utils_Tuple2(rawUrl, maybeTitle)))
					});
			};
			var maybeRawUrl = $pablohirafuji$elm_markdown$Markdown$Helpers$returnFirstJust(
				_List_fromArray(
					[maybeRawUrlAngleBrackets, maybeRawUrlWithoutBrackets]));
			return $elm$core$Maybe$Just(
				toMatch(
					A2($elm$core$Maybe$withDefault, '', maybeRawUrl)));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$checkForInlineLinkTypeOrImageType = function (_v0) {
	var remainText = _v0.a;
	var tempMatch = _v0.b;
	var model = _v0.c;
	return A2(
		$elm$core$Result$fromMaybe,
		_Utils_Tuple3(remainText, tempMatch, model),
		A2(
			$elm$core$Maybe$map,
			$pablohirafuji$elm_markdown$Markdown$InlineParser$addMatch(model),
			A2(
				$elm$core$Maybe$andThen,
				A2($pablohirafuji$elm_markdown$Markdown$InlineParser$inlineLinkTypeOrImageTypeRegexToMatch, tempMatch, model),
				$elm$core$List$head(
					A3($elm$regex$Regex$findAtMost, 1, $pablohirafuji$elm_markdown$Markdown$InlineParser$inlineLinkTypeOrImageTypeRegex, remainText)))));
};
var $pablohirafuji$elm_markdown$Markdown$Helpers$insideSquareBracketRegex = '[^\\[\\]\\\\]*(?:\\\\.[^\\[\\]\\\\]*)*';
var $pablohirafuji$elm_markdown$Markdown$InlineParser$refLabelRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^\\[\\s*(' + ($pablohirafuji$elm_markdown$Markdown$Helpers$insideSquareBracketRegex + ')\\s*\\]')));
var $pablohirafuji$elm_markdown$Markdown$Helpers$whitespacesRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('[' + ($pablohirafuji$elm_markdown$Markdown$Helpers$whiteSpaceChars + ']+')));
var $pablohirafuji$elm_markdown$Markdown$Helpers$cleanWhitespaces = A2(
	$elm$core$Basics$composeR,
	$elm$core$String$trim,
	A2(
		$elm$regex$Regex$replace,
		$pablohirafuji$elm_markdown$Markdown$Helpers$whitespacesRegex,
		function (_v0) {
			return ' ';
		}));
var $pablohirafuji$elm_markdown$Markdown$Helpers$prepareRefLabel = A2($elm$core$Basics$composeR, $pablohirafuji$elm_markdown$Markdown$Helpers$cleanWhitespaces, $elm$core$String$toLower);
var $pablohirafuji$elm_markdown$Markdown$InlineParser$refRegexToMatch = F3(
	function (matchModel, model, maybeRegexMatch) {
		var regexMatchLength = A2(
			$elm$core$Maybe$withDefault,
			0,
			A2(
				$elm$core$Maybe$map,
				A2(
					$elm$core$Basics$composeR,
					function ($) {
						return $.a2;
					},
					$elm$core$String$length),
				maybeRegexMatch));
		var toMatch = function (urlTitle) {
			return _Utils_update(
				matchModel,
				{
					dE: matchModel.dE + regexMatchLength,
					bj: function () {
						var _v0 = matchModel.bj;
						if (_v0.$ === 5) {
							return $pablohirafuji$elm_markdown$Markdown$InlineParser$ImageType;
						} else {
							return $pablohirafuji$elm_markdown$Markdown$InlineParser$LinkType;
						}
					}()(
						$pablohirafuji$elm_markdown$Markdown$InlineParser$prepareUrlAndTitle(urlTitle))
				});
		};
		var refLabel = function (str) {
			return $elm$core$String$isEmpty(str) ? matchModel.t : str;
		}(
			A2(
				$elm$core$Maybe$withDefault,
				matchModel.t,
				A2(
					$elm$core$Maybe$withDefault,
					$elm$core$Maybe$Nothing,
					A2(
						$elm$core$Maybe$withDefault,
						$elm$core$Maybe$Nothing,
						A2(
							$elm$core$Maybe$map,
							A2(
								$elm$core$Basics$composeR,
								function ($) {
									return $.eo;
								},
								$elm$core$List$head),
							maybeRegexMatch)))));
		var maybeRefItem = A2(
			$elm$core$Dict$get,
			$pablohirafuji$elm_markdown$Markdown$Helpers$prepareRefLabel(refLabel),
			model.dg);
		return A2($elm$core$Maybe$map, toMatch, maybeRefItem);
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$checkForRefLinkTypeOrImageType = function (_v0) {
	var remainText = _v0.a;
	var tempMatch = _v0.b;
	var model = _v0.c;
	return A2(
		$elm$core$Result$fromMaybe,
		_Utils_Tuple3(remainText, tempMatch, model),
		A2(
			$elm$core$Maybe$map,
			$pablohirafuji$elm_markdown$Markdown$InlineParser$addMatch(model),
			A3(
				$pablohirafuji$elm_markdown$Markdown$InlineParser$refRegexToMatch,
				tempMatch,
				model,
				$elm$core$List$head(
					A3($elm$regex$Regex$findAtMost, 1, $pablohirafuji$elm_markdown$Markdown$InlineParser$refLabelRegex, remainText)))));
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$checkParsedAheadOverlapping = function (parser) {
	var _v0 = parser.b;
	if (!_v0.b) {
		return $elm$core$Result$Err(0);
	} else {
		var match = _v0.a;
		var remainMatches = _v0.b;
		var overlappingMatches = A2(
			$elm$core$List$filter,
			function (_v1) {
				var testMatch = _v1;
				return (_Utils_cmp(match.dE, testMatch.di) > 0) && (_Utils_cmp(match.dE, testMatch.dE) < 0);
			},
			remainMatches);
		return ($elm$core$List$isEmpty(remainMatches) || $elm$core$List$isEmpty(overlappingMatches)) ? $elm$core$Result$Ok(parser) : $elm$core$Result$Err(0);
	}
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$emailRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^([a-zA-Z0-9.!#$%&\'*+\\/=?^_`{|}~\\-]+@[a-zA-Z0-9](?:[a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])?)*)$'));
var $pablohirafuji$elm_markdown$Markdown$InlineParser$emailAutolinkTypeToMatch = function (_v0) {
	var match = _v0;
	return A2($elm$regex$Regex$contains, $pablohirafuji$elm_markdown$Markdown$InlineParser$emailRegex, match.t) ? $elm$core$Result$Ok(
		_Utils_update(
			match,
			{
				bj: $pablohirafuji$elm_markdown$Markdown$InlineParser$AutolinkType(
					_Utils_Tuple2(
						match.t,
						'mailto:' + $pablohirafuji$elm_markdown$Markdown$InlineParser$encodeUrl(match.t)))
			})) : $elm$core$Result$Err(match);
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$filterTokens = F2(
	function (filter, model) {
		return _Utils_update(
			model,
			{
				j: A2($elm$core$List$filter, filter, model.j)
			});
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$findToken = F2(
	function (isToken, tokens) {
		var search = F2(
			function (token, _v2) {
				var maybeToken = _v2.a;
				var innerTokens = _v2.b;
				var remainTokens = _v2.c;
				if (maybeToken.$ === 1) {
					return isToken(token) ? _Utils_Tuple3(
						$elm$core$Maybe$Just(token),
						innerTokens,
						_List_Nil) : _Utils_Tuple3(
						$elm$core$Maybe$Nothing,
						A2($elm$core$List$cons, token, innerTokens),
						_List_Nil);
				} else {
					return _Utils_Tuple3(
						maybeToken,
						innerTokens,
						A2($elm$core$List$cons, token, remainTokens));
				}
			});
		var _return = function (_v0) {
			var maybeToken = _v0.a;
			var innerTokens = _v0.b;
			var remainTokens = _v0.c;
			return A2(
				$elm$core$Maybe$map,
				function (token) {
					return _Utils_Tuple3(
						token,
						$elm$core$List$reverse(innerTokens),
						$elm$core$List$reverse(remainTokens));
				},
				maybeToken);
		};
		return _return(
			A3(
				$elm$core$List$foldl,
				search,
				_Utils_Tuple3($elm$core$Maybe$Nothing, _List_Nil, _List_Nil),
				tokens));
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$HtmlModel = F2(
	function (tag, attributes) {
		return {bC: attributes, be: tag};
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$HtmlToken = F2(
	function (a, b) {
		return {$: 5, a: a, b: b};
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$attributesFromRegex = function (regexMatch) {
	var _v0 = regexMatch.eo;
	_v0$2:
	while (true) {
		if (_v0.b && (!_v0.a.$)) {
			if (_v0.a.a === '') {
				return $elm$core$Maybe$Nothing;
			} else {
				if ((_v0.b.b && _v0.b.b.b) && _v0.b.b.b.b) {
					var name = _v0.a.a;
					var _v1 = _v0.b;
					var maybeDoubleQuotes = _v1.a;
					var _v2 = _v1.b;
					var maybeSingleQuotes = _v2.a;
					var _v3 = _v2.b;
					var maybeUnquoted = _v3.a;
					var maybeValue = $pablohirafuji$elm_markdown$Markdown$Helpers$returnFirstJust(
						_List_fromArray(
							[maybeDoubleQuotes, maybeSingleQuotes, maybeUnquoted]));
					return $elm$core$Maybe$Just(
						_Utils_Tuple2(name, maybeValue));
				} else {
					break _v0$2;
				}
			}
		} else {
			break _v0$2;
		}
	}
	return $elm$core$Maybe$Nothing;
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$htmlAttributesRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('([a-zA-Z:_][a-zA-Z0-9\\-_.:]*)(?: ?= ?(?:\"([^\"]*)\"|\'([^\']*)\'|([^\\s\"\'=<>`]*)))?'));
var $pablohirafuji$elm_markdown$Markdown$InlineParser$applyAttributesRegex = A2(
	$elm$core$Basics$composeR,
	$elm$regex$Regex$find($pablohirafuji$elm_markdown$Markdown$InlineParser$htmlAttributesRegex),
	$elm$core$List$filterMap($pablohirafuji$elm_markdown$Markdown$InlineParser$attributesFromRegex));
var $pablohirafuji$elm_markdown$Markdown$InlineParser$htmlFromRegex = F3(
	function (model, match, regexMatch) {
		var _v0 = regexMatch.eo;
		if ((((_v0.b && _v0.b.b) && (!_v0.b.a.$)) && _v0.b.b.b) && _v0.b.b.b.b) {
			var maybeClose = _v0.a;
			var _v1 = _v0.b;
			var tag = _v1.a.a;
			var _v2 = _v1.b;
			var maybeAttributes = _v2.a;
			var _v3 = _v2.b;
			var maybeSelfClosing = _v3.a;
			var updateModel = function (attrs) {
				return A2(
					$pablohirafuji$elm_markdown$Markdown$InlineParser$addToken,
					model,
					{
						eY: match.di,
						c: match.dE - match.di,
						g: A2(
							$pablohirafuji$elm_markdown$Markdown$InlineParser$HtmlToken,
							_Utils_eq(maybeClose, $elm$core$Maybe$Nothing) && _Utils_eq(maybeSelfClosing, $elm$core$Maybe$Nothing),
							A2($pablohirafuji$elm_markdown$Markdown$InlineParser$HtmlModel, tag, attrs))
					});
			};
			var filterAttributes = F2(
				function (attrs, allowed) {
					return A2(
						$elm$core$List$filter,
						function (attr) {
							return A2($elm$core$List$member, attr.a, allowed);
						},
						attrs);
				});
			var attributes = A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				A2($elm$core$Maybe$map, $pablohirafuji$elm_markdown$Markdown$InlineParser$applyAttributesRegex, maybeAttributes));
			var noAttributesInCloseTag = _Utils_eq(maybeClose, $elm$core$Maybe$Nothing) || ((!_Utils_eq(maybeClose, $elm$core$Maybe$Nothing)) && _Utils_eq(attributes, _List_Nil));
			var _v4 = model.e6.ea;
			switch (_v4.$) {
				case 0:
					return noAttributesInCloseTag ? $elm$core$Maybe$Just(
						updateModel(attributes)) : $elm$core$Maybe$Nothing;
				case 1:
					var allowedHtmlElements = _v4.a.dr;
					var allowedHtmlAttributes = _v4.a.dq;
					return (A2($elm$core$List$member, tag, allowedHtmlElements) && noAttributesInCloseTag) ? $elm$core$Maybe$Just(
						updateModel(
							A2(filterAttributes, attributes, allowedHtmlAttributes))) : $elm$core$Maybe$Nothing;
				default:
					return $elm$core$Maybe$Nothing;
			}
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$htmlRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^(\\/)?([a-zA-Z][a-zA-Z0-9\\-]*)(?:\\s+([^<>]*?))?(\\/)?$'));
var $pablohirafuji$elm_markdown$Markdown$InlineParser$htmlToToken = F2(
	function (model, _v0) {
		var match = _v0;
		var _v1 = model.e6.ea;
		if (_v1.$ === 2) {
			return $elm$core$Maybe$Nothing;
		} else {
			return A2(
				$elm$core$Maybe$andThen,
				A2($pablohirafuji$elm_markdown$Markdown$InlineParser$htmlFromRegex, model, match),
				$elm$core$List$head(
					A3($elm$regex$Regex$findAtMost, 1, $pablohirafuji$elm_markdown$Markdown$InlineParser$htmlRegex, match.t)));
		}
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$isCloseToken = F2(
	function (htmlModel, token) {
		var _v0 = token.g;
		if ((_v0.$ === 5) && (!_v0.a)) {
			var htmlModel_ = _v0.b;
			return _Utils_eq(htmlModel.be, htmlModel_.be);
		} else {
			return false;
		}
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$isCodeTokenPair = F2(
	function (closeToken, openToken) {
		var _v0 = openToken.g;
		if (!_v0.$) {
			var isEscaped = _v0.a;
			return isEscaped ? _Utils_eq(openToken.c - 1, closeToken.c) : _Utils_eq(openToken.c, closeToken.c);
		} else {
			return false;
		}
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$isLinkTypeOrImageOpenToken = function (token) {
	var _v0 = token.g;
	switch (_v0.$) {
		case 1:
			return true;
		case 2:
			return true;
		default:
			return false;
	}
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$isOpenEmphasisToken = F2(
	function (closeToken, openToken) {
		var _v0 = openToken.g;
		if (_v0.$ === 6) {
			var openChar = _v0.a;
			var _v1 = _v0.b;
			var openLR = _v1.a;
			var openRR = _v1.b;
			var _v2 = closeToken.g;
			if (_v2.$ === 6) {
				var closeChar = _v2.a;
				var _v3 = _v2.b;
				var closeLR = _v3.a;
				var closeRR = _v3.b;
				return _Utils_eq(openChar, closeChar) ? ((_Utils_eq(openLR, openRR) || _Utils_eq(closeLR, closeRR)) ? (!(!A2($elm$core$Basics$modBy, 3, closeToken.c + openToken.c))) : true) : false;
			} else {
				return false;
			}
		} else {
			return false;
		}
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$voidHtmlTags = _List_fromArray(
	['area', 'base', 'br', 'col', 'embed', 'hr', 'img', 'input', 'keygen', 'link', 'meta', 'param', 'source', 'track', 'wbr']);
var $pablohirafuji$elm_markdown$Markdown$InlineParser$isVoidTag = function (htmlModel) {
	return A2($elm$core$List$member, htmlModel.be, $pablohirafuji$elm_markdown$Markdown$InlineParser$voidHtmlTags);
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$HardLineBreakType = {$: 1};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$SoftLineBreakToken = {$: 7};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$reverseTokens = function (model) {
	return _Utils_update(
		model,
		{
			j: $elm$core$List$reverse(model.j)
		});
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$tokenToMatch = F2(
	function (token, type_) {
		return {dE: token.eY + token.c, b: _List_Nil, di: token.eY, t: '', aI: 0, N: 0, bj: type_};
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$lineBreakTTM = function (_v0) {
	lineBreakTTM:
	while (true) {
		var tokens = _v0.a;
		var model = _v0.b;
		if (!tokens.b) {
			return $pablohirafuji$elm_markdown$Markdown$InlineParser$reverseTokens(model);
		} else {
			var token = tokens.a;
			var tokensTail = tokens.b;
			if (_Utils_eq(token.g, $pablohirafuji$elm_markdown$Markdown$InlineParser$HardLineBreakToken) || (_Utils_eq(token.g, $pablohirafuji$elm_markdown$Markdown$InlineParser$SoftLineBreakToken) && model.e6.el)) {
				return $pablohirafuji$elm_markdown$Markdown$InlineParser$lineBreakTTM(
					function (b) {
						return _Utils_Tuple2(tokensTail, b);
					}(
						_Utils_update(
							model,
							{
								b: A2(
									$elm$core$List$cons,
									A2($pablohirafuji$elm_markdown$Markdown$InlineParser$tokenToMatch, token, $pablohirafuji$elm_markdown$Markdown$InlineParser$HardLineBreakType),
									model.b)
							})));
			} else {
				var $temp$_v0 = _Utils_Tuple2(
					tokensTail,
					A2($pablohirafuji$elm_markdown$Markdown$InlineParser$addToken, model, token));
				_v0 = $temp$_v0;
				continue lineBreakTTM;
			}
		}
	}
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$removeParsedAheadTokens = F2(
	function (tokensTail, parser) {
		var _v0 = parser.b;
		if (!_v0.b) {
			return _Utils_Tuple2(tokensTail, parser);
		} else {
			var match = _v0.a;
			return _Utils_Tuple2(
				A2(
					$elm$core$List$filter,
					function (token) {
						return _Utils_cmp(token.eY, match.dE) > -1;
					},
					tokensTail),
				parser);
		}
	});
var $elm$core$Result$toMaybe = function (result) {
	if (!result.$) {
		var v = result.a;
		return $elm$core$Maybe$Just(v);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$angleBracketsToMatch = F4(
	function (closeToken, isEscaped, model, _v24) {
		var openToken = _v24.a;
		var remainTokens = _v24.c;
		return function (result) {
			if (result.$ === 1) {
				var tempMatch = result.a;
				return (!isEscaped) ? A2(
					$pablohirafuji$elm_markdown$Markdown$InlineParser$htmlToToken,
					_Utils_update(
						model,
						{j: remainTokens}),
					tempMatch) : $elm$core$Result$toMaybe(result);
			} else {
				return $elm$core$Result$toMaybe(result);
			}
		}(
			A2(
				$elm$core$Result$map,
				function (newMatch) {
					return _Utils_update(
						model,
						{
							b: A2($elm$core$List$cons, newMatch, model.b),
							j: remainTokens
						});
				},
				A2(
					$pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
					$pablohirafuji$elm_markdown$Markdown$InlineParser$emailAutolinkTypeToMatch,
					$pablohirafuji$elm_markdown$Markdown$InlineParser$autolinkToMatch(
						A6(
							$pablohirafuji$elm_markdown$Markdown$InlineParser$tokenPairToMatch,
							model,
							function (s) {
								return s;
							},
							$pablohirafuji$elm_markdown$Markdown$InlineParser$CodeType,
							openToken,
							closeToken,
							_List_Nil)))));
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$codeAutolinkTypeHtmlTagTTM = function (_v21) {
	codeAutolinkTypeHtmlTagTTM:
	while (true) {
		var tokens = _v21.a;
		var model = _v21.b;
		if (!tokens.b) {
			return $pablohirafuji$elm_markdown$Markdown$InlineParser$reverseTokens(model);
		} else {
			var token = tokens.a;
			var tokensTail = tokens.b;
			var _v23 = token.g;
			switch (_v23.$) {
				case 0:
					var isEscaped = _v23.a;
					return $pablohirafuji$elm_markdown$Markdown$InlineParser$codeAutolinkTypeHtmlTagTTM(
						function (b) {
							return _Utils_Tuple2(tokensTail, b);
						}(
							A2(
								$elm$core$Maybe$withDefault,
								A2($pablohirafuji$elm_markdown$Markdown$InlineParser$addToken, model, token),
								A2(
									$elm$core$Maybe$map,
									A2($pablohirafuji$elm_markdown$Markdown$InlineParser$codeToMatch, token, model),
									A2(
										$pablohirafuji$elm_markdown$Markdown$InlineParser$findToken,
										$pablohirafuji$elm_markdown$Markdown$InlineParser$isCodeTokenPair(token),
										model.j)))));
				case 4:
					var isEscaped = _v23.a;
					return $pablohirafuji$elm_markdown$Markdown$InlineParser$codeAutolinkTypeHtmlTagTTM(
						function (b) {
							return _Utils_Tuple2(tokensTail, b);
						}(
							A2(
								$pablohirafuji$elm_markdown$Markdown$InlineParser$filterTokens,
								A2(
									$elm$core$Basics$composeR,
									function ($) {
										return $.g;
									},
									$elm$core$Basics$neq(
										$pablohirafuji$elm_markdown$Markdown$InlineParser$CharToken('<'))),
								A2(
									$elm$core$Maybe$withDefault,
									model,
									A2(
										$elm$core$Maybe$andThen,
										A3($pablohirafuji$elm_markdown$Markdown$InlineParser$angleBracketsToMatch, token, isEscaped, model),
										A2(
											$pablohirafuji$elm_markdown$Markdown$InlineParser$findToken,
											A2(
												$elm$core$Basics$composeR,
												function ($) {
													return $.g;
												},
												$elm$core$Basics$eq(
													$pablohirafuji$elm_markdown$Markdown$InlineParser$CharToken('<'))),
											model.j))))));
				default:
					var $temp$_v21 = _Utils_Tuple2(
						tokensTail,
						A2($pablohirafuji$elm_markdown$Markdown$InlineParser$addToken, model, token));
					_v21 = $temp$_v21;
					continue codeAutolinkTypeHtmlTagTTM;
			}
		}
	}
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$codeToMatch = F3(
	function (closeToken, model, _v20) {
		var openToken = _v20.a;
		var remainTokens = _v20.c;
		var updtOpenToken = _Utils_eq(
			openToken.g,
			$pablohirafuji$elm_markdown$Markdown$InlineParser$CodeToken(true)) ? _Utils_update(
			openToken,
			{eY: openToken.eY + 1, c: openToken.c - 1}) : openToken;
		return _Utils_update(
			model,
			{
				b: A2(
					$elm$core$List$cons,
					A6($pablohirafuji$elm_markdown$Markdown$InlineParser$tokenPairToMatch, model, $pablohirafuji$elm_markdown$Markdown$Helpers$cleanWhitespaces, $pablohirafuji$elm_markdown$Markdown$InlineParser$CodeType, updtOpenToken, closeToken, _List_Nil),
					model.b),
				j: remainTokens
			});
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$emphasisTTM = function (_v16) {
	emphasisTTM:
	while (true) {
		var tokens = _v16.a;
		var model = _v16.b;
		if (!tokens.b) {
			return $pablohirafuji$elm_markdown$Markdown$InlineParser$reverseTokens(model);
		} else {
			var token = tokens.a;
			var tokensTail = tokens.b;
			var _v18 = token.g;
			if (_v18.$ === 6) {
				var _char = _v18.a;
				var _v19 = _v18.b;
				var leftRank = _v19.a;
				var rightRank = _v19.b;
				if (_Utils_eq(leftRank, rightRank)) {
					if ((!(!rightRank)) && ((_char !== '_') || (rightRank === 1))) {
						return $pablohirafuji$elm_markdown$Markdown$InlineParser$emphasisTTM(
							A2(
								$elm$core$Maybe$withDefault,
								_Utils_Tuple2(
									tokensTail,
									A2($pablohirafuji$elm_markdown$Markdown$InlineParser$addToken, model, token)),
								A2(
									$elm$core$Maybe$map,
									A3($pablohirafuji$elm_markdown$Markdown$InlineParser$emphasisToMatch, token, tokensTail, model),
									A2(
										$pablohirafuji$elm_markdown$Markdown$InlineParser$findToken,
										$pablohirafuji$elm_markdown$Markdown$InlineParser$isOpenEmphasisToken(token),
										model.j))));
					} else {
						var $temp$_v16 = _Utils_Tuple2(tokensTail, model);
						_v16 = $temp$_v16;
						continue emphasisTTM;
					}
				} else {
					if (_Utils_cmp(leftRank, rightRank) < 0) {
						var $temp$_v16 = _Utils_Tuple2(
							tokensTail,
							A2($pablohirafuji$elm_markdown$Markdown$InlineParser$addToken, model, token));
						_v16 = $temp$_v16;
						continue emphasisTTM;
					} else {
						return $pablohirafuji$elm_markdown$Markdown$InlineParser$emphasisTTM(
							A2(
								$elm$core$Maybe$withDefault,
								_Utils_Tuple2(tokensTail, model),
								A2(
									$elm$core$Maybe$map,
									A3($pablohirafuji$elm_markdown$Markdown$InlineParser$emphasisToMatch, token, tokensTail, model),
									A2(
										$pablohirafuji$elm_markdown$Markdown$InlineParser$findToken,
										$pablohirafuji$elm_markdown$Markdown$InlineParser$isOpenEmphasisToken(token),
										model.j))));
					}
				}
			} else {
				var $temp$_v16 = _Utils_Tuple2(
					tokensTail,
					A2($pablohirafuji$elm_markdown$Markdown$InlineParser$addToken, model, token));
				_v16 = $temp$_v16;
				continue emphasisTTM;
			}
		}
	}
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$emphasisToMatch = F4(
	function (closeToken, tokensTail, model, _v15) {
		var openToken = _v15.a;
		var innerTokens = _v15.b;
		var remainTokens = _v15.c;
		var remainLength = openToken.c - closeToken.c;
		var updt = (!remainLength) ? {bJ: closeToken, a6: openToken, cx: remainTokens, cJ: tokensTail} : ((remainLength > 0) ? {
			bJ: closeToken,
			a6: _Utils_update(
				openToken,
				{eY: openToken.eY + remainLength, c: closeToken.c}),
			cx: A2(
				$elm$core$List$cons,
				_Utils_update(
					openToken,
					{c: remainLength}),
				remainTokens),
			cJ: tokensTail
		} : {
			bJ: _Utils_update(
				closeToken,
				{c: openToken.c}),
			a6: openToken,
			cx: remainTokens,
			cJ: A2(
				$elm$core$List$cons,
				_Utils_update(
					closeToken,
					{eY: closeToken.eY + openToken.c, c: -remainLength}),
				tokensTail)
		});
		var match = A6(
			$pablohirafuji$elm_markdown$Markdown$InlineParser$tokenPairToMatch,
			model,
			function (s) {
				return s;
			},
			$pablohirafuji$elm_markdown$Markdown$InlineParser$EmphasisType(updt.a6.c),
			updt.a6,
			updt.bJ,
			$elm$core$List$reverse(innerTokens));
		return _Utils_Tuple2(
			updt.cJ,
			_Utils_update(
				model,
				{
					b: A2($elm$core$List$cons, match, model.b),
					j: updt.cx
				}));
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$htmlElementTTM = function (_v12) {
	htmlElementTTM:
	while (true) {
		var tokens = _v12.a;
		var model = _v12.b;
		if (!tokens.b) {
			return $pablohirafuji$elm_markdown$Markdown$InlineParser$reverseTokens(model);
		} else {
			var token = tokens.a;
			var tokensTail = tokens.b;
			var _v14 = token.g;
			if (_v14.$ === 5) {
				var isOpen = _v14.a;
				var htmlModel = _v14.b;
				return ($pablohirafuji$elm_markdown$Markdown$InlineParser$isVoidTag(htmlModel) || (!isOpen)) ? $pablohirafuji$elm_markdown$Markdown$InlineParser$htmlElementTTM(
					function (b) {
						return _Utils_Tuple2(tokensTail, b);
					}(
						A2(
							$pablohirafuji$elm_markdown$Markdown$InlineParser$addMatch,
							model,
							A2(
								$pablohirafuji$elm_markdown$Markdown$InlineParser$tokenToMatch,
								token,
								$pablohirafuji$elm_markdown$Markdown$InlineParser$HtmlType(htmlModel))))) : $pablohirafuji$elm_markdown$Markdown$InlineParser$htmlElementTTM(
					A2(
						$elm$core$Maybe$withDefault,
						function (b) {
							return _Utils_Tuple2(tokensTail, b);
						}(
							A2(
								$pablohirafuji$elm_markdown$Markdown$InlineParser$addMatch,
								model,
								A2(
									$pablohirafuji$elm_markdown$Markdown$InlineParser$tokenToMatch,
									token,
									$pablohirafuji$elm_markdown$Markdown$InlineParser$HtmlType(htmlModel)))),
						A2(
							$elm$core$Maybe$map,
							A3($pablohirafuji$elm_markdown$Markdown$InlineParser$htmlElementToMatch, token, model, htmlModel),
							A2(
								$pablohirafuji$elm_markdown$Markdown$InlineParser$findToken,
								$pablohirafuji$elm_markdown$Markdown$InlineParser$isCloseToken(htmlModel),
								tokensTail))));
			} else {
				var $temp$_v12 = _Utils_Tuple2(
					tokensTail,
					A2($pablohirafuji$elm_markdown$Markdown$InlineParser$addToken, model, token));
				_v12 = $temp$_v12;
				continue htmlElementTTM;
			}
		}
	}
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$htmlElementToMatch = F4(
	function (openToken, model, htmlModel, _v11) {
		var closeToken = _v11.a;
		var innerTokens = _v11.b;
		var remainTokens = _v11.c;
		return _Utils_Tuple2(
			remainTokens,
			_Utils_update(
				model,
				{
					b: A2(
						$elm$core$List$cons,
						A6(
							$pablohirafuji$elm_markdown$Markdown$InlineParser$tokenPairToMatch,
							model,
							function (s) {
								return s;
							},
							$pablohirafuji$elm_markdown$Markdown$InlineParser$HtmlType(htmlModel),
							openToken,
							closeToken,
							innerTokens),
						model.b)
				}));
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$linkImageTypeTTM = function (_v8) {
	linkImageTypeTTM:
	while (true) {
		var tokens = _v8.a;
		var model = _v8.b;
		if (!tokens.b) {
			return $pablohirafuji$elm_markdown$Markdown$InlineParser$reverseTokens(model);
		} else {
			var token = tokens.a;
			var tokensTail = tokens.b;
			var _v10 = token.g;
			if ((_v10.$ === 3) && (']' === _v10.a)) {
				return $pablohirafuji$elm_markdown$Markdown$InlineParser$linkImageTypeTTM(
					A2(
						$elm$core$Maybe$withDefault,
						_Utils_Tuple2(tokensTail, model),
						A2(
							$elm$core$Maybe$andThen,
							A3($pablohirafuji$elm_markdown$Markdown$InlineParser$linkOrImageTypeToMatch, token, tokensTail, model),
							A2($pablohirafuji$elm_markdown$Markdown$InlineParser$findToken, $pablohirafuji$elm_markdown$Markdown$InlineParser$isLinkTypeOrImageOpenToken, model.j))));
			} else {
				var $temp$_v8 = _Utils_Tuple2(
					tokensTail,
					A2($pablohirafuji$elm_markdown$Markdown$InlineParser$addToken, model, token));
				_v8 = $temp$_v8;
				continue linkImageTypeTTM;
			}
		}
	}
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$linkOrImageTypeToMatch = F4(
	function (closeToken, tokensTail, model, _v1) {
		var openToken = _v1.a;
		var innerTokens = _v1.b;
		var remainTokens = _v1.c;
		var tempMatch = function (isLinkType) {
			return A6(
				$pablohirafuji$elm_markdown$Markdown$InlineParser$tokenPairToMatch,
				model,
				function (s) {
					return s;
				},
				isLinkType ? $pablohirafuji$elm_markdown$Markdown$InlineParser$LinkType(
					_Utils_Tuple2('', $elm$core$Maybe$Nothing)) : $pablohirafuji$elm_markdown$Markdown$InlineParser$ImageType(
					_Utils_Tuple2('', $elm$core$Maybe$Nothing)),
				openToken,
				closeToken,
				$elm$core$List$reverse(innerTokens));
		};
		var removeOpenToken = _Utils_Tuple2(
			tokensTail,
			_Utils_update(
				model,
				{
					j: _Utils_ap(innerTokens, remainTokens)
				}));
		var remainText = A2($elm$core$String$dropLeft, closeToken.eY + 1, model.B);
		var linkOpenTokenToInactive = function (model_) {
			var process = function (token) {
				var _v7 = token.g;
				if (_v7.$ === 1) {
					return _Utils_update(
						token,
						{
							g: $pablohirafuji$elm_markdown$Markdown$InlineParser$LinkOpenToken(false)
						});
				} else {
					return token;
				}
			};
			return _Utils_update(
				model_,
				{
					j: A2($elm$core$List$map, process, model_.j)
				});
		};
		var args = function (isLinkType) {
			return _Utils_Tuple3(
				remainText,
				tempMatch(isLinkType),
				_Utils_update(
					model,
					{j: remainTokens}));
		};
		var _v2 = openToken.g;
		switch (_v2.$) {
			case 2:
				return $elm$core$Result$toMaybe(
					A2(
						$pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
						function (_v4) {
							return $elm$core$Result$Ok(removeOpenToken);
						},
						A2(
							$elm$core$Result$map,
							$pablohirafuji$elm_markdown$Markdown$InlineParser$removeParsedAheadTokens(tokensTail),
							A2(
								$elm$core$Result$andThen,
								$pablohirafuji$elm_markdown$Markdown$InlineParser$checkParsedAheadOverlapping,
								A2(
									$elm$core$Result$mapError,
									function (_v3) {
										return 0;
									},
									A2(
										$pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
										$pablohirafuji$elm_markdown$Markdown$InlineParser$checkForRefLinkTypeOrImageType,
										$pablohirafuji$elm_markdown$Markdown$InlineParser$checkForInlineLinkTypeOrImageType(
											args(false))))))));
			case 1:
				if (_v2.a) {
					return $elm$core$Result$toMaybe(
						A2(
							$pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
							function (_v6) {
								return $elm$core$Result$Ok(removeOpenToken);
							},
							A2(
								$elm$core$Result$map,
								$pablohirafuji$elm_markdown$Markdown$InlineParser$removeParsedAheadTokens(tokensTail),
								A2(
									$elm$core$Result$map,
									linkOpenTokenToInactive,
									A2(
										$elm$core$Result$andThen,
										$pablohirafuji$elm_markdown$Markdown$InlineParser$checkParsedAheadOverlapping,
										A2(
											$elm$core$Result$mapError,
											function (_v5) {
												return 0;
											},
											A2(
												$pablohirafuji$elm_markdown$Markdown$Helpers$ifError,
												$pablohirafuji$elm_markdown$Markdown$InlineParser$checkForRefLinkTypeOrImageType,
												$pablohirafuji$elm_markdown$Markdown$InlineParser$checkForInlineLinkTypeOrImageType(
													args(true)))))))));
				} else {
					return $elm$core$Maybe$Just(removeOpenToken);
				}
			default:
				return $elm$core$Maybe$Nothing;
		}
	});
var $pablohirafuji$elm_markdown$Markdown$InlineParser$tokenPairToMatch = F6(
	function (model, processText, type_, openToken, closeToken, innerTokens) {
		var textStart = openToken.eY + openToken.c;
		var textEnd = closeToken.eY;
		var start = openToken.eY;
		var end = closeToken.eY + closeToken.c;
		var match = {
			dE: end,
			b: _List_Nil,
			di: start,
			t: processText(
				A3($elm$core$String$slice, textStart, textEnd, model.B)),
			aI: textEnd,
			N: textStart,
			bj: type_
		};
		var matches = A2(
			$elm$core$List$map,
			function (_v0) {
				var matchModel = _v0;
				return A2($pablohirafuji$elm_markdown$Markdown$InlineParser$prepareChildMatch, match, matchModel);
			},
			$pablohirafuji$elm_markdown$Markdown$InlineParser$cyclic$tokensToMatches()(
				_Utils_update(
					model,
					{b: _List_Nil, j: innerTokens})).b);
		return _Utils_update(
			match,
			{b: matches});
	});
function $pablohirafuji$elm_markdown$Markdown$InlineParser$cyclic$tokensToMatches() {
	return A2(
		$elm$core$Basics$composeR,
		$pablohirafuji$elm_markdown$Markdown$InlineParser$applyTTM($pablohirafuji$elm_markdown$Markdown$InlineParser$codeAutolinkTypeHtmlTagTTM),
		A2(
			$elm$core$Basics$composeR,
			$pablohirafuji$elm_markdown$Markdown$InlineParser$applyTTM($pablohirafuji$elm_markdown$Markdown$InlineParser$htmlElementTTM),
			A2(
				$elm$core$Basics$composeR,
				$pablohirafuji$elm_markdown$Markdown$InlineParser$applyTTM($pablohirafuji$elm_markdown$Markdown$InlineParser$linkImageTypeTTM),
				A2(
					$elm$core$Basics$composeR,
					$pablohirafuji$elm_markdown$Markdown$InlineParser$applyTTM($pablohirafuji$elm_markdown$Markdown$InlineParser$emphasisTTM),
					$pablohirafuji$elm_markdown$Markdown$InlineParser$applyTTM($pablohirafuji$elm_markdown$Markdown$InlineParser$lineBreakTTM)))));
}
var $pablohirafuji$elm_markdown$Markdown$InlineParser$tokensToMatches = $pablohirafuji$elm_markdown$Markdown$InlineParser$cyclic$tokensToMatches();
$pablohirafuji$elm_markdown$Markdown$InlineParser$cyclic$tokensToMatches = function () {
	return $pablohirafuji$elm_markdown$Markdown$InlineParser$tokensToMatches;
};
var $pablohirafuji$elm_markdown$Markdown$InlineParser$parse = F3(
	function (options, refs, rawText) {
		return $pablohirafuji$elm_markdown$Markdown$InlineParser$matchesToInlines(
			$pablohirafuji$elm_markdown$Markdown$InlineParser$parseText(
				$pablohirafuji$elm_markdown$Markdown$InlineParser$organizeParserMatches(
					$pablohirafuji$elm_markdown$Markdown$InlineParser$tokensToMatches(
						$pablohirafuji$elm_markdown$Markdown$InlineParser$tokenize(
							A3(
								$pablohirafuji$elm_markdown$Markdown$InlineParser$initParser,
								options,
								refs,
								$elm$core$String$trim(rawText)))))).b);
	});
var $pablohirafuji$elm_markdown$Markdown$Block$parseInline = F4(
	function (maybeOptions, textAsParagraph, refs, block) {
		var options = A2($elm$core$Maybe$withDefault, $pablohirafuji$elm_markdown$Markdown$Config$defaultOptions, maybeOptions);
		switch (block.$) {
			case 2:
				var rawText = block.a;
				var lvl = block.b;
				return A3(
					$pablohirafuji$elm_markdown$Markdown$Block$Heading,
					rawText,
					lvl,
					A3($pablohirafuji$elm_markdown$Markdown$InlineParser$parse, options, refs, rawText));
			case 4:
				var rawText = block.a;
				var inlines = A3($pablohirafuji$elm_markdown$Markdown$InlineParser$parse, options, refs, rawText);
				if ((inlines.b && (inlines.a.$ === 5)) && (!inlines.b.b)) {
					var _v3 = inlines.a;
					return $pablohirafuji$elm_markdown$Markdown$Block$PlainInlines(inlines);
				} else {
					return textAsParagraph ? A2($pablohirafuji$elm_markdown$Markdown$Block$Paragraph, rawText, inlines) : $pablohirafuji$elm_markdown$Markdown$Block$PlainInlines(inlines);
				}
			case 5:
				var blocks = block.a;
				return $pablohirafuji$elm_markdown$Markdown$Block$BlockQuote(
					A3(
						$pablohirafuji$elm_markdown$Markdown$Block$parseInlines,
						maybeOptions,
						true,
						_Utils_Tuple2(refs, blocks)));
			case 6:
				var model = block.a;
				var items = block.b;
				return A2(
					$pablohirafuji$elm_markdown$Markdown$Block$List,
					model,
					function (a) {
						return A2($elm$core$List$map, a, items);
					}(
						A2(
							$elm$core$Basics$composeL,
							A2($pablohirafuji$elm_markdown$Markdown$Block$parseInlines, maybeOptions, model.W),
							function (b) {
								return _Utils_Tuple2(refs, b);
							})));
			case 8:
				var customBlock = block.a;
				var blocks = block.b;
				return A2(
					$pablohirafuji$elm_markdown$Markdown$Block$Custom,
					customBlock,
					A3(
						$pablohirafuji$elm_markdown$Markdown$Block$parseInlines,
						maybeOptions,
						true,
						_Utils_Tuple2(refs, blocks)));
			default:
				return block;
		}
	});
var $pablohirafuji$elm_markdown$Markdown$Block$parseInlines = F3(
	function (maybeOptions, textAsParagraph, _v0) {
		var refs = _v0.a;
		var blocks = _v0.b;
		return A2(
			$elm$core$List$map,
			A3($pablohirafuji$elm_markdown$Markdown$Block$parseInline, maybeOptions, textAsParagraph, refs),
			blocks);
	});
var $elm$core$Tuple$mapSecond = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			x,
			func(y));
	});
var $pablohirafuji$elm_markdown$Markdown$Block$dropRefString = F2(
	function (rawText, inlineMatch) {
		var strippedText = A2($elm$core$String$dropLeft, inlineMatch.c6, rawText);
		return A2($elm$regex$Regex$contains, $pablohirafuji$elm_markdown$Markdown$Block$blankLineRegex, strippedText) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(strippedText);
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (!_v0.$) {
			return true;
		} else {
			return false;
		}
	});
var $pablohirafuji$elm_markdown$Markdown$Block$insertLinkMatch = F2(
	function (refs, linkMatch) {
		return A2($elm$core$Dict$member, linkMatch.af, refs) ? refs : A3(
			$elm$core$Dict$insert,
			linkMatch.af,
			_Utils_Tuple2(linkMatch.v, linkMatch.c7),
			refs);
	});
var $pablohirafuji$elm_markdown$Markdown$Block$extractUrlTitleRegex = function (regexMatch) {
	var _v0 = regexMatch.eo;
	if ((((((_v0.b && (!_v0.a.$)) && _v0.b.b) && _v0.b.b.b) && _v0.b.b.b.b) && _v0.b.b.b.b.b) && _v0.b.b.b.b.b.b) {
		var rawText = _v0.a.a;
		var _v1 = _v0.b;
		var maybeRawUrlAngleBrackets = _v1.a;
		var _v2 = _v1.b;
		var maybeRawUrlWithoutBrackets = _v2.a;
		var _v3 = _v2.b;
		var maybeTitleSingleQuotes = _v3.a;
		var _v4 = _v3.b;
		var maybeTitleDoubleQuotes = _v4.a;
		var _v5 = _v4.b;
		var maybeTitleParenthesis = _v5.a;
		var toReturn = function (rawUrl) {
			return {
				af: rawText,
				c6: $elm$core$String$length(regexMatch.a2),
				c7: $pablohirafuji$elm_markdown$Markdown$Helpers$returnFirstJust(
					_List_fromArray(
						[maybeTitleSingleQuotes, maybeTitleDoubleQuotes, maybeTitleParenthesis])),
				v: rawUrl
			};
		};
		var maybeRawUrl = $pablohirafuji$elm_markdown$Markdown$Helpers$returnFirstJust(
			_List_fromArray(
				[maybeRawUrlAngleBrackets, maybeRawUrlWithoutBrackets]));
		return A2($elm$core$Maybe$map, toReturn, maybeRawUrl);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $pablohirafuji$elm_markdown$Markdown$Block$hrefRegex = '\\s*(?:<([^<>\\s]*)>|([^\\s]*))';
var $pablohirafuji$elm_markdown$Markdown$Block$refRegex = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	$elm$regex$Regex$fromString('^\\s*\\[(' + ($pablohirafuji$elm_markdown$Markdown$Helpers$insideSquareBracketRegex + (')\\]:' + ($pablohirafuji$elm_markdown$Markdown$Block$hrefRegex + ($pablohirafuji$elm_markdown$Markdown$Helpers$titleRegex + '\\s*(?![^\\n])'))))));
var $pablohirafuji$elm_markdown$Markdown$Block$maybeLinkMatch = function (rawText) {
	return A2(
		$elm$core$Maybe$andThen,
		function (linkMatch) {
			return ((linkMatch.v === '') || (linkMatch.af === '')) ? $elm$core$Maybe$Nothing : $elm$core$Maybe$Just(linkMatch);
		},
		A2(
			$elm$core$Maybe$map,
			function (linkMatch) {
				return _Utils_update(
					linkMatch,
					{
						af: $pablohirafuji$elm_markdown$Markdown$Helpers$prepareRefLabel(linkMatch.af)
					});
			},
			A2(
				$elm$core$Maybe$andThen,
				$pablohirafuji$elm_markdown$Markdown$Block$extractUrlTitleRegex,
				$elm$core$List$head(
					A3($elm$regex$Regex$findAtMost, 1, $pablohirafuji$elm_markdown$Markdown$Block$refRegex, rawText)))));
};
var $pablohirafuji$elm_markdown$Markdown$Block$parseReference = F2(
	function (refs, rawText) {
		parseReference:
		while (true) {
			var _v0 = $pablohirafuji$elm_markdown$Markdown$Block$maybeLinkMatch(rawText);
			if (!_v0.$) {
				var linkMatch = _v0.a;
				var updtRefs = A2($pablohirafuji$elm_markdown$Markdown$Block$insertLinkMatch, refs, linkMatch);
				var maybeStrippedText = A2($pablohirafuji$elm_markdown$Markdown$Block$dropRefString, rawText, linkMatch);
				if (!maybeStrippedText.$) {
					var strippedText = maybeStrippedText.a;
					var $temp$refs = updtRefs,
						$temp$rawText = strippedText;
					refs = $temp$refs;
					rawText = $temp$rawText;
					continue parseReference;
				} else {
					return _Utils_Tuple2(updtRefs, $elm$core$Maybe$Nothing);
				}
			} else {
				return _Utils_Tuple2(
					refs,
					$elm$core$Maybe$Just(rawText));
			}
		}
	});
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $pablohirafuji$elm_markdown$Markdown$Block$parseReferences = function (refs) {
	return A2(
		$elm$core$List$foldl,
		$pablohirafuji$elm_markdown$Markdown$Block$parseReferencesHelp,
		_Utils_Tuple2(refs, _List_Nil));
};
var $pablohirafuji$elm_markdown$Markdown$Block$parseReferencesHelp = F2(
	function (block, _v0) {
		var refs = _v0.a;
		var parsedAST = _v0.b;
		switch (block.$) {
			case 4:
				var rawText = block.a;
				var _v2 = A2($pablohirafuji$elm_markdown$Markdown$Block$parseReference, $elm$core$Dict$empty, rawText);
				var paragraphRefs = _v2.a;
				var maybeUpdtText = _v2.b;
				var updtRefs = A2($elm$core$Dict$union, paragraphRefs, refs);
				if (!maybeUpdtText.$) {
					var updtText = maybeUpdtText.a;
					return _Utils_Tuple2(
						updtRefs,
						A2(
							$elm$core$List$cons,
							A2($pablohirafuji$elm_markdown$Markdown$Block$Paragraph, updtText, _List_Nil),
							parsedAST));
				} else {
					return _Utils_Tuple2(updtRefs, parsedAST);
				}
			case 6:
				var model = block.a;
				var items = block.b;
				var _v4 = A3(
					$elm$core$List$foldl,
					F2(
						function (item, _v5) {
							var refs__ = _v5.a;
							var parsedItems = _v5.b;
							return A2(
								$elm$core$Tuple$mapSecond,
								function (a) {
									return A2($elm$core$List$cons, a, parsedItems);
								},
								A2($pablohirafuji$elm_markdown$Markdown$Block$parseReferences, refs__, item));
						}),
					_Utils_Tuple2(refs, _List_Nil),
					items);
				var updtRefs = _v4.a;
				var updtItems = _v4.b;
				return _Utils_Tuple2(
					updtRefs,
					A2(
						$elm$core$List$cons,
						A2($pablohirafuji$elm_markdown$Markdown$Block$List, model, updtItems),
						parsedAST));
			case 5:
				var blocks = block.a;
				return A2(
					$elm$core$Tuple$mapSecond,
					function (a) {
						return A2($elm$core$List$cons, a, parsedAST);
					},
					A2(
						$elm$core$Tuple$mapSecond,
						$pablohirafuji$elm_markdown$Markdown$Block$BlockQuote,
						A2($pablohirafuji$elm_markdown$Markdown$Block$parseReferences, refs, blocks)));
			case 8:
				var customBlock = block.a;
				var blocks = block.b;
				return A2(
					$elm$core$Tuple$mapSecond,
					function (a) {
						return A2($elm$core$List$cons, a, parsedAST);
					},
					A2(
						$elm$core$Tuple$mapSecond,
						$pablohirafuji$elm_markdown$Markdown$Block$Custom(customBlock),
						A2($pablohirafuji$elm_markdown$Markdown$Block$parseReferences, refs, blocks)));
			default:
				return _Utils_Tuple2(
					refs,
					A2($elm$core$List$cons, block, parsedAST));
		}
	});
var $pablohirafuji$elm_markdown$Markdown$Block$parse = function (maybeOptions) {
	return A2(
		$elm$core$Basics$composeR,
		$elm$core$String$lines,
		A2(
			$elm$core$Basics$composeR,
			function (a) {
				return A2($pablohirafuji$elm_markdown$Markdown$Block$incorporateLines, a, _List_Nil);
			},
			A2(
				$elm$core$Basics$composeR,
				$pablohirafuji$elm_markdown$Markdown$Block$parseReferences($elm$core$Dict$empty),
				A2($pablohirafuji$elm_markdown$Markdown$Block$parseInlines, maybeOptions, true))));
};
var $elm$html$Html$blockquote = _VirtualDom_node('blockquote');
var $elm$html$Html$code = _VirtualDom_node('code');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$h1 = _VirtualDom_node('h1');
var $elm$html$Html$h2 = _VirtualDom_node('h2');
var $elm$html$Html$h4 = _VirtualDom_node('h4');
var $elm$html$Html$h5 = _VirtualDom_node('h5');
var $elm$html$Html$h6 = _VirtualDom_node('h6');
var $elm$html$Html$hr = _VirtualDom_node('hr');
var $elm$html$Html$li = _VirtualDom_node('li');
var $elm$html$Html$ol = _VirtualDom_node('ol');
var $elm$html$Html$pre = _VirtualDom_node('pre');
var $elm$html$Html$Attributes$start = function (n) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'start',
		$elm$core$String$fromInt(n));
};
var $elm$html$Html$Attributes$alt = $elm$html$Html$Attributes$stringProperty('alt');
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $pablohirafuji$elm_markdown$Markdown$Inline$attributeToAttribute = function (_v0) {
	var name = _v0.a;
	var maybeValue = _v0.b;
	return A2(
		$elm$html$Html$Attributes$attribute,
		name,
		A2($elm$core$Maybe$withDefault, name, maybeValue));
};
var $pablohirafuji$elm_markdown$Markdown$Inline$attributesToHtmlAttributes = $elm$core$List$map($pablohirafuji$elm_markdown$Markdown$Inline$attributeToAttribute);
var $elm$html$Html$br = _VirtualDom_node('br');
var $elm$html$Html$em = _VirtualDom_node('em');
var $pablohirafuji$elm_markdown$Markdown$Inline$extractText = function (inlines) {
	return A3($elm$core$List$foldl, $pablohirafuji$elm_markdown$Markdown$Inline$extractTextHelp, '', inlines);
};
var $pablohirafuji$elm_markdown$Markdown$Inline$extractTextHelp = F2(
	function (inline, text) {
		switch (inline.$) {
			case 0:
				var str = inline.a;
				return _Utils_ap(text, str);
			case 1:
				return text + ' ';
			case 2:
				var str = inline.a;
				return _Utils_ap(text, str);
			case 3:
				var inlines = inline.c;
				return _Utils_ap(
					text,
					$pablohirafuji$elm_markdown$Markdown$Inline$extractText(inlines));
			case 4:
				var inlines = inline.c;
				return _Utils_ap(
					text,
					$pablohirafuji$elm_markdown$Markdown$Inline$extractText(inlines));
			case 5:
				var inlines = inline.c;
				return _Utils_ap(
					text,
					$pablohirafuji$elm_markdown$Markdown$Inline$extractText(inlines));
			case 6:
				var inlines = inline.b;
				return _Utils_ap(
					text,
					$pablohirafuji$elm_markdown$Markdown$Inline$extractText(inlines));
			default:
				var inlines = inline.b;
				return _Utils_ap(
					text,
					$pablohirafuji$elm_markdown$Markdown$Inline$extractText(inlines));
		}
	});
var $elm$html$Html$img = _VirtualDom_node('img');
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $elm$html$Html$Attributes$src = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var $elm$html$Html$strong = _VirtualDom_node('strong');
var $elm$html$Html$Attributes$title = $elm$html$Html$Attributes$stringProperty('title');
var $pablohirafuji$elm_markdown$Markdown$Inline$defaultHtml = F2(
	function (customTransformer, inline) {
		var transformer = A2(
			$elm$core$Maybe$withDefault,
			$pablohirafuji$elm_markdown$Markdown$Inline$defaultHtml($elm$core$Maybe$Nothing),
			customTransformer);
		switch (inline.$) {
			case 0:
				var str = inline.a;
				return $elm$html$Html$text(str);
			case 1:
				return A2($elm$html$Html$br, _List_Nil, _List_Nil);
			case 2:
				var codeStr = inline.a;
				return A2(
					$elm$html$Html$code,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(codeStr)
						]));
			case 3:
				var url = inline.a;
				var maybeTitle = inline.b;
				var inlines = inline.c;
				if (!maybeTitle.$) {
					var title_ = maybeTitle.a;
					return A2(
						$elm$html$Html$a,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$href(url),
								$elm$html$Html$Attributes$title(title_)
							]),
						A2($elm$core$List$map, transformer, inlines));
				} else {
					return A2(
						$elm$html$Html$a,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$href(url)
							]),
						A2($elm$core$List$map, transformer, inlines));
				}
			case 4:
				var url = inline.a;
				var maybeTitle = inline.b;
				var inlines = inline.c;
				if (!maybeTitle.$) {
					var title_ = maybeTitle.a;
					return A2(
						$elm$html$Html$img,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$alt(
								$pablohirafuji$elm_markdown$Markdown$Inline$extractText(inlines)),
								$elm$html$Html$Attributes$src(url),
								$elm$html$Html$Attributes$title(title_)
							]),
						_List_Nil);
				} else {
					return A2(
						$elm$html$Html$img,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$alt(
								$pablohirafuji$elm_markdown$Markdown$Inline$extractText(inlines)),
								$elm$html$Html$Attributes$src(url)
							]),
						_List_Nil);
				}
			case 5:
				var tag = inline.a;
				var attrs = inline.b;
				var inlines = inline.c;
				return A3(
					$elm$html$Html$node,
					tag,
					$pablohirafuji$elm_markdown$Markdown$Inline$attributesToHtmlAttributes(attrs),
					A2($elm$core$List$map, transformer, inlines));
			case 6:
				var length = inline.a;
				var inlines = inline.b;
				switch (length) {
					case 1:
						return A2(
							$elm$html$Html$em,
							_List_Nil,
							A2($elm$core$List$map, transformer, inlines));
					case 2:
						return A2(
							$elm$html$Html$strong,
							_List_Nil,
							A2($elm$core$List$map, transformer, inlines));
					default:
						return ((length - 2) > 0) ? A2(
							$elm$html$Html$strong,
							_List_Nil,
							function (a) {
								return A2($elm$core$List$cons, a, _List_Nil);
							}(
								transformer(
									A2($pablohirafuji$elm_markdown$Markdown$Inline$Emphasis, length - 2, inlines)))) : A2(
							$elm$html$Html$em,
							_List_Nil,
							A2($elm$core$List$map, transformer, inlines));
				}
			default:
				var inlines = inline.b;
				return $elm$html$Html$text('');
		}
	});
var $pablohirafuji$elm_markdown$Markdown$Inline$toHtml = $pablohirafuji$elm_markdown$Markdown$Inline$defaultHtml($elm$core$Maybe$Nothing);
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $pablohirafuji$elm_markdown$Markdown$Block$defaultHtml = F3(
	function (customHtml, customInlineHtml, block) {
		var inlineToHtml = A2($elm$core$Maybe$withDefault, $pablohirafuji$elm_markdown$Markdown$Inline$toHtml, customInlineHtml);
		var blockToHtml = A2(
			$elm$core$Maybe$withDefault,
			A2($pablohirafuji$elm_markdown$Markdown$Block$defaultHtml, $elm$core$Maybe$Nothing, customInlineHtml),
			customHtml);
		switch (block.$) {
			case 0:
				return _List_Nil;
			case 2:
				var level = block.b;
				var inlines = block.c;
				var hElement = function () {
					switch (level) {
						case 1:
							return $elm$html$Html$h1(_List_Nil);
						case 2:
							return $elm$html$Html$h2(_List_Nil);
						case 3:
							return $elm$html$Html$h3(_List_Nil);
						case 4:
							return $elm$html$Html$h4(_List_Nil);
						case 5:
							return $elm$html$Html$h5(_List_Nil);
						default:
							return $elm$html$Html$h6(_List_Nil);
					}
				}();
				return _List_fromArray(
					[
						hElement(
						A2($elm$core$List$map, inlineToHtml, inlines))
					]);
			case 1:
				return _List_fromArray(
					[
						A2($elm$html$Html$hr, _List_Nil, _List_Nil)
					]);
			case 4:
				var inlines = block.b;
				return _List_fromArray(
					[
						A2(
						$elm$html$Html$p,
						_List_Nil,
						A2($elm$core$List$map, inlineToHtml, inlines))
					]);
			case 3:
				if (block.a.$ === 1) {
					var _v2 = block.a;
					var model = _v2.b;
					var codeStr = block.b;
					var basicView = function (attrs) {
						return _List_fromArray(
							[
								A2(
								$elm$html$Html$pre,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$code,
										attrs,
										_List_fromArray(
											[
												$elm$html$Html$text(codeStr)
											]))
									]))
							]);
					};
					var _v3 = model.c5;
					if (!_v3.$) {
						var language = _v3.a;
						return basicView(
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('language-' + language)
								]));
					} else {
						return basicView(_List_Nil);
					}
				} else {
					var _v4 = block.a;
					var codeStr = block.b;
					return _List_fromArray(
						[
							A2(
							$elm$html$Html$pre,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$code,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(codeStr)
										]))
								]))
						]);
				}
			case 5:
				var blocks = block.a;
				return function (a) {
					return A2($elm$core$List$cons, a, _List_Nil);
				}(
					A2(
						$elm$html$Html$blockquote,
						_List_Nil,
						$elm$core$List$concat(
							A2($elm$core$List$map, blockToHtml, blocks))));
			case 6:
				var model = block.a;
				var items = block.b;
				return function (a) {
					return A2($elm$core$List$cons, a, _List_Nil);
				}(
					function () {
						var _v5 = model.bj;
						if (_v5.$ === 1) {
							var startInt = _v5.a;
							return (startInt === 1) ? $elm$html$Html$ol(_List_Nil) : $elm$html$Html$ol(
								_List_fromArray(
									[
										$elm$html$Html$Attributes$start(startInt)
									]));
						} else {
							return $elm$html$Html$ul(_List_Nil);
						}
					}()(
						A2(
							$elm$core$List$map,
							A2(
								$elm$core$Basics$composeR,
								$elm$core$List$map(blockToHtml),
								A2(
									$elm$core$Basics$composeR,
									$elm$core$List$concat,
									$elm$html$Html$li(_List_Nil))),
							items)));
			case 7:
				var inlines = block.a;
				return A2($elm$core$List$map, inlineToHtml, inlines);
			default:
				var customBlock = block.a;
				var blocks = block.b;
				return function (a) {
					return A2($elm$core$List$cons, a, _List_Nil);
				}(
					A2(
						$elm$html$Html$div,
						_List_Nil,
						A2(
							$elm$core$List$cons,
							$elm$html$Html$text('Unhandled custom block.'),
							$elm$core$List$concat(
								A2($elm$core$List$map, blockToHtml, blocks)))));
		}
	});
var $pablohirafuji$elm_markdown$Markdown$Block$toHtml = A2($pablohirafuji$elm_markdown$Markdown$Block$defaultHtml, $elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing);
var $pablohirafuji$elm_markdown$Markdown$toHtml = F2(
	function (maybeOptions, rawText) {
		return $elm$core$List$concat(
			A2(
				$elm$core$List$map,
				$pablohirafuji$elm_markdown$Markdown$Block$toHtml,
				A2($pablohirafuji$elm_markdown$Markdown$Block$parse, maybeOptions, rawText)));
	});
var $author$project$Main$setContent = F2(
	function (response, content) {
		if (response.$ === 1) {
			var err = response.a;
			return {
				S: _List_fromArray(
					[
						$elm$html$Html$text(
						$author$project$Main$errorToString(err))
					]),
				U: '',
				P: 'HTTP request ERROR',
				v: ''
			};
		} else {
			var raw = response.a;
			return _Utils_update(
				content,
				{
					S: A2($pablohirafuji$elm_markdown$Markdown$toHtml, $elm$core$Maybe$Nothing, raw)
				});
		}
	});
var $elm$url$Url$addPort = F2(
	function (maybePort, starter) {
		if (maybePort.$ === 1) {
			return starter;
		} else {
			var port_ = maybePort.a;
			return starter + (':' + $elm$core$String$fromInt(port_));
		}
	});
var $elm$url$Url$addPrefixed = F3(
	function (prefix, maybeSegment, starter) {
		if (maybeSegment.$ === 1) {
			return starter;
		} else {
			var segment = maybeSegment.a;
			return _Utils_ap(
				starter,
				_Utils_ap(prefix, segment));
		}
	});
var $elm$url$Url$toString = function (url) {
	var http = function () {
		var _v0 = url.d8;
		if (!_v0) {
			return 'http://';
		} else {
			return 'https://';
		}
	}();
	return A3(
		$elm$url$Url$addPrefixed,
		'#',
		url.dI,
		A3(
			$elm$url$Url$addPrefixed,
			'?',
			url.d9,
			_Utils_ap(
				A2(
					$elm$url$Url$addPort,
					url.d3,
					_Utils_ap(http, url.dJ)),
				url.d1)));
};
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 0:
				var content = msg.a;
				var response = msg.b;
				return $author$project$Main$loadNext(
					_Utils_update(
						model,
						{
							aR: _Utils_ap(
								model.aR,
								_List_fromArray(
									[
										A2($author$project$Main$setContent, response, content)
									]))
						}));
			case 1:
				var state = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{an: state}),
					$elm$core$Platform$Cmd$none);
			case 2:
				var s = msg.a;
				var idx = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							a0: A3(
								$elm$core$Array$set,
								idx,
								$author$project$Main$convert(s),
								model.a0)
						}),
					$elm$core$Platform$Cmd$none);
			case 3:
				var request = msg.a;
				if (!request.$) {
					var url = request.a;
					return _Utils_Tuple2(
						model,
						A2(
							$elm$browser$Browser$Navigation$pushUrl,
							model.c4,
							$elm$url$Url$toString(url)));
				} else {
					var href = request.a;
					return _Utils_Tuple2(
						model,
						$elm$browser$Browser$Navigation$load(href));
				}
			default:
				var url = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{v: url}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$BlockAttrs = function (a) {
	return {$: 3, a: a};
};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Block$attrs = function (attrs_) {
	return $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$BlockAttrs(attrs_);
};
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $author$project$Main$bg = $elm$html$Html$Attributes$style('background-color');
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Config = $elm$core$Basics$identity;
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$CardBlock = function (a) {
	return {$: 0, a: a};
};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$applyBlockModifier = F2(
	function (option, options) {
		switch (option.$) {
			case 0:
				var align = option.a;
				return _Utils_update(
					options,
					{
						Q: $elm$core$Maybe$Just(align)
					});
			case 1:
				var role = option.a;
				return _Utils_update(
					options,
					{
						T: $elm$core$Maybe$Just(role)
					});
			case 2:
				var color = option.a;
				return _Utils_update(
					options,
					{
						_: $elm$core$Maybe$Just(color)
					});
			default:
				var attrs = option.a;
				return _Utils_update(
					options,
					{
						bC: _Utils_ap(options.bC, attrs)
					});
		}
	});
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$defaultBlockOptions = {Q: $elm$core$Maybe$Nothing, bC: _List_Nil, T: $elm$core$Maybe$Nothing, _: $elm$core$Maybe$Nothing};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption = function (size) {
	switch (size) {
		case 0:
			return $elm$core$Maybe$Nothing;
		case 1:
			return $elm$core$Maybe$Just('sm');
		case 2:
			return $elm$core$Maybe$Just('md');
		case 3:
			return $elm$core$Maybe$Just('lg');
		default:
			return $elm$core$Maybe$Just('xl');
	}
};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Internal$Text$textAlignDirOption = function (dir) {
	switch (dir) {
		case 1:
			return 'center';
		case 0:
			return 'left';
		default:
			return 'right';
	}
};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Internal$Text$textAlignClass = function (_v0) {
	var dir = _v0.dC;
	var size = _v0.cD;
	return $elm$html$Html$Attributes$class(
		'text' + (A2(
			$elm$core$Maybe$withDefault,
			'-',
			A2(
				$elm$core$Maybe$map,
				function (s) {
					return '-' + (s + '-');
				},
				$EdutainmentLIVE$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(size))) + $EdutainmentLIVE$elm_bootstrap$Bootstrap$Internal$Text$textAlignDirOption(dir)));
};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Internal$Role$toClass = F2(
	function (prefix, role) {
		return $elm$html$Html$Attributes$class(
			prefix + ('-' + function () {
				switch (role) {
					case 0:
						return 'primary';
					case 1:
						return 'secondary';
					case 2:
						return 'success';
					case 3:
						return 'info';
					case 4:
						return 'warning';
					case 5:
						return 'danger';
					case 6:
						return 'light';
					default:
						return 'dark';
				}
			}()));
	});
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Internal$Text$textColorClass = function (color) {
	if (color.$ === 1) {
		return $elm$html$Html$Attributes$class('text-white');
	} else {
		var role = color.a;
		return A2($EdutainmentLIVE$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'text', role);
	}
};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$blockAttributes = function (modifiers) {
	var options = A3($elm$core$List$foldl, $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$applyBlockModifier, $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$defaultBlockOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('card-body')
			]),
		_Utils_ap(
			function () {
				var _v0 = options.Q;
				if (!_v0.$) {
					var align = _v0.a;
					return _List_fromArray(
						[
							$EdutainmentLIVE$elm_bootstrap$Bootstrap$Internal$Text$textAlignClass(align)
						]);
				} else {
					return _List_Nil;
				}
			}(),
			_Utils_ap(
				function () {
					var _v1 = options.T;
					if (!_v1.$) {
						var role = _v1.a;
						return _List_fromArray(
							[
								A2($EdutainmentLIVE$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'bg', role)
							]);
					} else {
						return _List_Nil;
					}
				}(),
				_Utils_ap(
					function () {
						var _v2 = options._;
						if (!_v2.$) {
							var color = _v2.a;
							return _List_fromArray(
								[
									$EdutainmentLIVE$elm_bootstrap$Bootstrap$Internal$Text$textColorClass(color)
								]);
						} else {
							return _List_Nil;
						}
					}(),
					options.bC))));
};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$block = F2(
	function (options, items) {
		return $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$CardBlock(
			A2(
				$elm$html$Html$div,
				$EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$blockAttributes(options),
				A2(
					$elm$core$List$map,
					function (_v0) {
						var e = _v0;
						return e;
					},
					items)));
	});
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$block = F3(
	function (options, items, _v0) {
		var conf = _v0;
		return _Utils_update(
			conf,
			{
				eG: _Utils_ap(
					conf.eG,
					_List_fromArray(
						[
							A2($EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$block, options, items)
						]))
			});
	});
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$config = function (options) {
	return {eG: _List_Nil, c$: $elm$core$Maybe$Nothing, eR: $elm$core$Maybe$Nothing, c1: $elm$core$Maybe$Nothing, c2: $elm$core$Maybe$Nothing, e6: options};
};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$BlockItem = $elm$core$Basics$identity;
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Block$custom = function (element) {
	return element;
};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Footer = $elm$core$Basics$identity;
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$footer = F3(
	function (attributes, children, _v0) {
		var conf = _v0;
		return _Utils_update(
			conf,
			{
				c$: $elm$core$Maybe$Just(
					A2(
						$elm$html$Html$div,
						A2(
							$elm$core$List$cons,
							$elm$html$Html$Attributes$class('card-footer'),
							attributes),
						children))
			});
	});
var $author$project$Main$ft = $elm$html$Html$Attributes$style('font-family');
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Header = $elm$core$Basics$identity;
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$headerPrivate = F4(
	function (elemFn, attributes, children, _v0) {
		var conf = _v0;
		return _Utils_update(
			conf,
			{
				eR: $elm$core$Maybe$Just(
					A2(
						elemFn,
						A2(
							$elm$core$List$cons,
							$elm$html$Html$Attributes$class('card-header'),
							attributes),
						children))
			});
	});
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$headerH1 = $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$headerPrivate($elm$html$Html$h1);
var $author$project$Main$Convert = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $elm$core$Elm$JsArray$foldl = _JsArray_foldl;
var $elm$core$Elm$JsArray$indexedMap = _JsArray_indexedMap;
var $elm$core$Array$indexedMap = F2(
	function (func, _v0) {
		var len = _v0.a;
		var tree = _v0.c;
		var tail = _v0.d;
		var initialBuilder = {
			r: _List_Nil,
			m: 0,
			n: A3(
				$elm$core$Elm$JsArray$indexedMap,
				func,
				$elm$core$Array$tailIndex(len),
				tail)
		};
		var helper = F2(
			function (node, builder) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldl, helper, builder, subTree);
				} else {
					var leaf = node.a;
					var offset = builder.m * $elm$core$Array$branchFactor;
					var mappedLeaf = $elm$core$Array$Leaf(
						A3($elm$core$Elm$JsArray$indexedMap, func, offset, leaf));
					return {
						r: A2($elm$core$List$cons, mappedLeaf, builder.r),
						m: builder.m + 1,
						n: builder.n
					};
				}
			});
		return A2(
			$elm$core$Array$builderToArray,
			true,
			A3($elm$core$Elm$JsArray$foldl, helper, initialBuilder, tree));
	});
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onMouseOver = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'mouseover',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$html$Html$span = _VirtualDom_node('span');
var $author$project$Main$makeTitle = function (lmdexpr) {
	return A2(
		$elm$core$List$cons,
		$elm$html$Html$text(
			$elm$core$String$fromChar('\u03BB') + 'x. { '),
		_Utils_ap(
			$elm$core$Array$toList(
				A2(
					$elm$core$Array$indexedMap,
					F2(
						function (idx, s) {
							return A2(
								$elm$html$Html$span,
								_List_fromArray(
									[
										$elm$html$Html$Events$onMouseOver(
										A2($author$project$Main$Convert, s, idx))
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(s + ' ')
									]));
						}),
					lmdexpr)),
			_List_fromArray(
				[
					$elm$html$Html$text('}')
				])));
};
var $author$project$Main$noto = 'Noto Sans JP, sans-serif';
var $author$project$Main$roboto = 'Roboto Condensed, sans-serif';
var $elm$html$Html$Attributes$rel = _VirtualDom_attribute('rel');
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$CDN$stylesheet = A3(
	$elm$html$Html$node,
	'link',
	_List_fromArray(
		[
			$elm$html$Html$Attributes$rel('stylesheet'),
			$elm$html$Html$Attributes$href('https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css')
		]),
	_List_Nil);
var $author$project$Main$tc = $elm$html$Html$Attributes$style('color');
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$applyModifier = F2(
	function (option, options) {
		switch (option.$) {
			case 0:
				var align = option.a;
				return _Utils_update(
					options,
					{
						Q: $elm$core$Maybe$Just(align)
					});
			case 1:
				var coloring = option.a;
				return _Utils_update(
					options,
					{
						T: $elm$core$Maybe$Just(coloring)
					});
			case 2:
				var coloring = option.a;
				return _Utils_update(
					options,
					{
						_: $elm$core$Maybe$Just(coloring)
					});
			default:
				var attrs = option.a;
				return _Utils_update(
					options,
					{
						bC: _Utils_ap(options.bC, attrs)
					});
		}
	});
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$defaultOptions = {Q: $elm$core$Maybe$Nothing, bC: _List_Nil, T: $elm$core$Maybe$Nothing, _: $elm$core$Maybe$Nothing};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$cardAttributes = function (modifiers) {
	var options = A3($elm$core$List$foldl, $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$applyModifier, $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$defaultOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('card')
			]),
		_Utils_ap(
			function () {
				var _v0 = options.T;
				if (!_v0.$) {
					if (!_v0.a.$) {
						var role = _v0.a.a;
						return _List_fromArray(
							[
								A2($EdutainmentLIVE$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'bg', role)
							]);
					} else {
						var role = _v0.a.a;
						return _List_fromArray(
							[
								A2($EdutainmentLIVE$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'border', role)
							]);
					}
				} else {
					return _List_Nil;
				}
			}(),
			_Utils_ap(
				function () {
					var _v1 = options._;
					if (!_v1.$) {
						var color = _v1.a;
						return _List_fromArray(
							[
								$EdutainmentLIVE$elm_bootstrap$Bootstrap$Internal$Text$textColorClass(color)
							]);
					} else {
						return _List_Nil;
					}
				}(),
				_Utils_ap(
					function () {
						var _v2 = options.Q;
						if (!_v2.$) {
							var align = _v2.a;
							return _List_fromArray(
								[
									$EdutainmentLIVE$elm_bootstrap$Bootstrap$Internal$Text$textAlignClass(align)
								]);
						} else {
							return _List_Nil;
						}
					}(),
					options.bC))));
};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$renderBlocks = function (blocks) {
	return A2(
		$elm$core$List$map,
		function (block_) {
			if (!block_.$) {
				var e = block_.a;
				return e;
			} else {
				var e = block_.a;
				return e;
			}
		},
		blocks);
};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$view = function (_v0) {
	var conf = _v0;
	return A2(
		$elm$html$Html$div,
		$EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$cardAttributes(conf.e6),
		_Utils_ap(
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2(
						$elm$core$Maybe$map,
						function (_v1) {
							var e = _v1;
							return e;
						},
						conf.eR),
						A2(
						$elm$core$Maybe$map,
						function (_v2) {
							var e = _v2;
							return e;
						},
						conf.c2)
					])),
			_Utils_ap(
				$EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$renderBlocks(conf.eG),
				A2(
					$elm$core$List$filterMap,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							A2(
							$elm$core$Maybe$map,
							function (_v3) {
								var e = _v3;
								return e;
							},
							conf.c$),
							A2(
							$elm$core$Maybe$map,
							function (_v4) {
								var e = _v4;
								return e;
							},
							conf.c1)
						])))));
};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$block = $EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$block;
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$Card = $elm$core$Basics$identity;
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$card = function (rec) {
	return {eG: rec.eG, eR: rec.eR, eW: rec.eW, e6: rec.e6};
};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$Config = $elm$core$Basics$identity;
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$cards = F2(
	function (cards_, _v0) {
		var configRec = _v0;
		return _Utils_update(
			configRec,
			{bF: cards_});
	});
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$config = function (toMsg) {
	return {bF: _List_Nil, a5: false, dm: toMsg, al: false};
};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$Header = $elm$core$Basics$identity;
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$headerPrivate = F3(
	function (elemFn, attributes, toggle_) {
		return {bC: attributes, bH: _List_Nil, bI: _List_Nil, dD: elemFn, et: toggle_};
	});
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$header = $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$headerPrivate($elm$html$Html$div);
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$onlyOneOpen = function (_v0) {
	var configRec = _v0;
	return _Utils_update(
		configRec,
		{a5: true});
};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$Toggle = $elm$core$Basics$identity;
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$toggle = F2(
	function (attributes, children) {
		return {bC: attributes, dw: children};
	});
var $elm$core$String$fromFloat = _String_fromNumber;
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$getOrInitCardState = F2(
	function (id, _v0) {
		var cardStates = _v0;
		return A2(
			$elm$core$Maybe$withDefault,
			{at: $elm$core$Maybe$Nothing, w: 0},
			A2($elm$core$Dict$get, id, cardStates));
	});
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$transitionStyle = F2(
	function (withAnimation_, height) {
		return _Utils_ap(
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'position', 'relative'),
					A2($elm$html$Html$Attributes$style, 'height', height),
					A2($elm$html$Html$Attributes$style, 'overflow', 'hidden')
				]),
			withAnimation_ ? _List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, '-webkit-transition-timing-function', 'ease'),
					A2($elm$html$Html$Attributes$style, '-o-transition-timing-function', 'ease'),
					A2($elm$html$Html$Attributes$style, 'transition-timing-function', 'ease'),
					A2($elm$html$Html$Attributes$style, '-webkit-transition-duration', '0.35s'),
					A2($elm$html$Html$Attributes$style, '-o-transition-duration', '0.35s'),
					A2($elm$html$Html$Attributes$style, 'transition-duration', '0.35s'),
					A2($elm$html$Html$Attributes$style, '-webkit-transition-property', 'height'),
					A2($elm$html$Html$Attributes$style, '-o-transition-property', 'height'),
					A2($elm$html$Html$Attributes$style, 'transition-property', 'height')
				]) : _List_Nil);
	});
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$animationAttributes = F3(
	function (state, configRec, _v0) {
		var id = _v0.eW;
		var styles = $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$transitionStyle(configRec.al);
		var cardState = A2($EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$getOrInitCardState, id, state);
		var pixelHeight = A2(
			$elm$core$Maybe$withDefault,
			'0',
			A2(
				$elm$core$Maybe$map,
				function (v) {
					return $elm$core$String$fromFloat(v) + 'px';
				},
				cardState.at));
		var _v1 = cardState.w;
		switch (_v1) {
			case 0:
				return styles('0px');
			case 1:
				return styles('0px');
			case 2:
				return styles(pixelHeight);
			default:
				var _v2 = cardState.at;
				if (!_v2.$) {
					var x = _v2.a;
					return styles(pixelHeight);
				} else {
					return styles('100%');
				}
		}
	});
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$renderCardBlock = F3(
	function (state, configRec, card_) {
		var id = card_.eW;
		var blocks = card_.eG;
		return A2(
			$elm$html$Html$div,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id(id)
					]),
				A3($EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$animationAttributes, state, configRec, card_)),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_Nil,
					$EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$renderBlocks(blocks))
				]));
	});
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$visibilityTransition = F2(
	function (withAnimation_, visibility) {
		var _v0 = _Utils_Tuple2(withAnimation_, visibility);
		if (_v0.a) {
			switch (_v0.b) {
				case 0:
					var _v1 = _v0.b;
					return 1;
				case 1:
					var _v2 = _v0.b;
					return 3;
				case 3:
					var _v3 = _v0.b;
					return 2;
				default:
					var _v4 = _v0.b;
					return 0;
			}
		} else {
			switch (_v0.b) {
				case 0:
					var _v5 = _v0.b;
					return 3;
				case 3:
					var _v6 = _v0.b;
					return 0;
				default:
					return 3;
			}
		}
	});
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$clickHandler = F4(
	function (state, configRec, decoder, _v0) {
		var cardStates = state;
		var id = _v0.eW;
		var currentCardState = A2(
			$elm$core$Maybe$withDefault,
			{at: $elm$core$Maybe$Nothing, w: 0},
			A2($elm$core$Dict$get, id, cardStates));
		var initStates = A3($elm$core$Dict$insert, id, currentCardState, cardStates);
		var updOthersHidden = function (h) {
			return A2(
				$elm$core$Dict$map,
				F2(
					function (i, c) {
						return _Utils_eq(i, id) ? {
							at: $elm$core$Maybe$Just(h),
							w: A2($EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$visibilityTransition, configRec.al, c.w)
						} : (((c.w === 3) && (configRec.al && configRec.a5)) ? _Utils_update(
							c,
							{w: 2}) : (((c.w === 3) && ((!configRec.al) && configRec.a5)) ? _Utils_update(
							c,
							{w: 0}) : c));
					}),
				initStates);
		};
		return A2(
			$elm$json$Json$Decode$andThen,
			function (v) {
				return $elm$json$Json$Decode$succeed(
					{
						e0: configRec.dm(
							updOthersHidden(v)),
						e7: true,
						fg: true
					});
			},
			decoder);
	});
var $elm$virtual_dom$VirtualDom$Custom = function (a) {
	return {$: 3, a: a};
};
var $elm$html$Html$Events$custom = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Custom(decoder));
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Utilities$DomHelper$childNode = function (idx) {
	return $elm$json$Json$Decode$at(
		_List_fromArray(
			[
				'childNodes',
				$elm$core$String$fromInt(idx)
			]));
};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Utilities$DomHelper$nextSibling = function (decoder) {
	return A2($elm$json$Json$Decode$field, 'nextSibling', decoder);
};
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetHeight = A2($elm$json$Json$Decode$field, 'offsetHeight', $elm$json$Json$Decode$float);
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Utilities$DomHelper$parentElement = function (decoder) {
	return A2($elm$json$Json$Decode$field, 'parentElement', decoder);
};
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$heightDecoder = A2(
	$elm$json$Json$Decode$field,
	'currentTarget',
	$EdutainmentLIVE$elm_bootstrap$Bootstrap$Utilities$DomHelper$parentElement(
		$EdutainmentLIVE$elm_bootstrap$Bootstrap$Utilities$DomHelper$nextSibling(
			A2($EdutainmentLIVE$elm_bootstrap$Bootstrap$Utilities$DomHelper$childNode, 0, $EdutainmentLIVE$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetHeight))));
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$renderToggle = F3(
	function (state, configRec, card_) {
		var cardRec = card_;
		var _v0 = cardRec.eR;
		var headerRec = _v0;
		var _v1 = headerRec.et;
		var attributes = _v1.bC;
		var children = _v1.dw;
		return A2(
			$elm$html$Html$button,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('btn btn-link'),
						A2(
						$elm$html$Html$Events$custom,
						'click',
						A4($EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$clickHandler, state, configRec, $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$heightDecoder, card_))
					]),
				attributes),
			children);
	});
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$renderCardHeader = F3(
	function (state, configRec, card_) {
		var cardRec = card_;
		var _v0 = cardRec.eR;
		var headerRec = _v0;
		return A2(
			headerRec.dD,
			_Utils_ap(
				headerRec.bC,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('card-header')
					])),
			_Utils_ap(
				headerRec.bI,
				_Utils_ap(
					_List_fromArray(
						[
							A3($EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$renderToggle, state, configRec, card_)
						]),
					headerRec.bH)));
	});
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$renderCard = F3(
	function (state, configRec, card_) {
		var options = card_.e6;
		return A2(
			$elm$html$Html$div,
			_Utils_ap(
				$EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Internal$cardAttributes(options),
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('card')
					])),
			_List_fromArray(
				[
					A3($EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$renderCardHeader, state, configRec, card_),
					A3($EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$renderCardBlock, state, configRec, card_)
				]));
	});
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$view = F2(
	function (state, _v0) {
		var configRec = _v0;
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			A2(
				$elm$core$List$map,
				A2($EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$renderCard, state, configRec),
				configRec.bF));
	});
var $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$withAnimation = function (_v0) {
	var configRec = _v0;
	return _Utils_update(
		configRec,
		{al: true});
};
var $author$project$Main$viewLoaded = function (model) {
	return A2(
		$EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$view,
		model.an,
		A2(
			$EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$cards,
			A2(
				$elm$core$List$map,
				function (content) {
					return $EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$card(
						{
							eG: _List_fromArray(
								[
									A2(
									$EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$block,
									_List_Nil,
									_List_fromArray(
										[
											$EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Block$custom(
											A2($elm$html$Html$div, _List_Nil, content.S))
										]))
								]),
							eR: A2(
								$EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$header,
								_List_fromArray(
									[
										$author$project$Main$bg('#444444')
									]),
								A2(
									$EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$toggle,
									_List_fromArray(
										[
											A2($elm$html$Html$Attributes$style, 'width', '100%'),
											A2($elm$html$Html$Attributes$style, 'text-align', 'left')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$a,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$href('?content=' + content.v),
													A2($elm$html$Html$Attributes$style, 'padding', '0')
												]),
											_List_fromArray(
												[
													A2(
													$elm$html$Html$div,
													_List_fromArray(
														[
															$author$project$Main$bg('#444444'),
															$author$project$Main$tc('#eeeeee'),
															A2($elm$html$Html$Attributes$style, 'overflow', 'hidden'),
															A2($elm$html$Html$Attributes$style, 'white-space', 'nowrap'),
															A2($elm$html$Html$Attributes$style, 'text-overflow', 'ellipsis')
														]),
													_List_fromArray(
														[
															$elm$html$Html$text('[' + (content.U + ('] ' + content.P)))
														]))
												]))
										]))),
							eW: content.v,
							e6: _List_Nil
						});
				},
				model.aR),
			$EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$onlyOneOpen(
				$EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$withAnimation(
					$EdutainmentLIVE$elm_bootstrap$Bootstrap$Accordion$config($author$project$Main$AccordionMsg)))));
};
var $author$project$Main$viewLoading = A2(
	$elm$html$Html$div,
	_List_fromArray(
		[
			A2($elm$html$Html$Attributes$style, 'text-align', 'center'),
			$author$project$Main$ft($author$project$Main$noto),
			$author$project$Main$tc('#ccccaa')
		]),
	_List_fromArray(
		[
			A2(
			$elm$html$Html$i,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('fa fa-spinner fa-spin')
				]),
			_List_Nil),
			$elm$html$Html$text(' Loading ...')
		]));
var $author$project$Main$view = function (model) {
	return {
		S: _List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('responsive')
					]),
				_List_fromArray(
					[
						$EdutainmentLIVE$elm_bootstrap$Bootstrap$CDN$stylesheet,
						$EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$view(
						A3(
							$EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$block,
							_List_fromArray(
								[
									$EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Block$attrs(
									_List_fromArray(
										[
											$author$project$Main$bg('#222233'),
											$author$project$Main$ft($author$project$Main$noto)
										]))
								]),
							_List_fromArray(
								[
									$EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$Block$custom(
									$elm$core$List$isEmpty(model.a1) ? $author$project$Main$viewLoaded(model) : $author$project$Main$viewLoading)
								]),
							A3(
								$EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$footer,
								_List_fromArray(
									[
										$author$project$Main$bg('#222233'),
										$author$project$Main$tc('#ccccaa'),
										$author$project$Main$ft($author$project$Main$noto),
										A2($elm$html$Html$Attributes$style, 'text-align', 'right')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('(c) 2020 '),
										A2(
										$elm$html$Html$a,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$href('http://lmdexpr.com')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Yuki Tajiri(lmdexpr)')
											]))
									]),
								A3(
									$EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$headerH1,
									_List_fromArray(
										[
											$author$project$Main$bg('#222233'),
											$author$project$Main$tc('#eeeeaa'),
											$author$project$Main$ft($author$project$Main$roboto)
										]),
									$author$project$Main$makeTitle(model.a0),
									$EdutainmentLIVE$elm_bootstrap$Bootstrap$Card$config(_List_Nil)))))
					]))
			]),
		P: 'lmdexpr.com'
	};
};
var $author$project$Main$main = $elm$browser$Browser$application(
	{eZ: $author$project$Main$init, e3: $author$project$Main$UrlChanged, e4: $author$project$Main$LinkClicked, fh: $author$project$Main$subscriptions, fj: $author$project$Main$update, fk: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main($elm$json$Json$Decode$value)(0)}});}(this));