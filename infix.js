#!/usr/bin/env node

var program = require('commander');
var math = require('mathjs');
var fs = require('fs');

//定义参数,以及参数内容的描述
program
.version('0.0.1')
.usage('[options] [value ...]')
.option('-m, --messag <string>', 'a string argument')
.option('-f, --filename <string>', 'a string argument')
.parse(process.argv);
//解析commandline arguments


CONSTANTS = ["PI", "E"]


SECRETFUNCTIONS = {"^": "pow", "**": "pow", "abs": "fabs"}


function writeFile(file,str){  
      
    // appendFile，如果文件不存在，会自动创建新文件  
    // 如果用writeFile，那么会删除旧文件，直接写新文件  
    fs.writeFile(file, str, function(err){  
        if(err)  
            console.log("fail " + err);  
        else  
            console.log("写入文件ok");  
    });  
}

function bottom_up(tree, cb) {
    if (tree.args||tree.type == "ParenthesisNode") {
	while(tree.type == "ParenthesisNode"){tree = tree.content;}
        tree.args = tree.args.map(function(node) {return bottom_up(node, cb)});
        tree.res = cb(tree);
    } else {
        tree.res = cb(tree);
    }
    return tree;
}

function dump_tree(tree) /* tree -> string */ {
    function extract(args) {return args.map(function(n) {return n.res});}
    var names = [];
    var body = bottom_up(tree, function(node) {
        switch(node.type) {
        case "ConstantNode":
            return "" + node.value;
        case "FunctionNode":
            return "(" + node.name + " " + extract(node.args).join(" ") + ")";
        case "OperatorNode":
            node.op = SECRETFUNCTIONS[node.op] || node.op;
            return "(" + node.op + " " + extract(node.args).join(" ") + ")";
        case "SymbolNode":
            if (CONSTANTS.indexOf(node.name) === -1)
                names.push(node.name);
            return node.name;
	case "ParenthesisNode":
	    return "(" + node.content.op + " " + node.content.args.join(" ") + ")";
        default:
            throw SyntaxError("Invalid tree!");
        }
    });

    var dnames = [];
    for (var i = 0; i < names.length; i++) {
        if (dnames.indexOf(names[i]) === -1) dnames.push(names[i]);
    }
    return "(FPCore (" + dnames.join(" ") + ") " + body.res + ")";
}

function on_load(str){
	var txt = str;
	var tree,lisp;
	tree = math.parse(str);
	lisp = dump_tree(tree);
	return lisp
}

to_fpcore = on_load(program.messag);
writeFile(program.filename,to_fpcore);




