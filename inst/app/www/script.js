function formatarPorcentagem(value) {
  return Intl.NumberFormat('pt-BR', { 
    style: 'percent', 
    minimumFractionDigits: 0, 
    maximumFractionDigits: 2
  }).format(value/100);
}

function formatarNumero(value) {
  return Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(value);
}

function formatarDinheiro(num, digits = 2) {
  
  var value = Intl.NumberFormat('pt-BR', { 
    style: 'decimal', 
    minimumFractionDigits: 0, 
    maximumFractionDigits: digits,
  }).format(num);
  
  return value; 
}

//function formatarDinheiro(num, digits = 2) {
//  var si = [
//    { value: 1, symbol: "" },
//    { value: 1E6, symbol: "M" },
//    { value: 1E9, symbol: "B" },
//    { value: 1E12, symbol: "T" }
//  ];
//  var rx = /\.0+$|(\.[0-9]*[1-9])0+$/;
//  var i;
//  for (i = si.length - 1; i > 0; i--) {
//    if (num >= si[i].value) {
//      break;
//    }
//  }
//  return "R$ " + (num / si[i].value).toFixed(digits).replace(rx, "$1").replace//(".", ",") + si[i].symbol;
//}

function formatarTexto(str) {
  return (str.length > 30) ? str.substr(0, 29) + '...' : str;
}

function formatarTexto2(str) {
  return (str.length > 20) ? str.substr(0, 19) + '...' : str;
}

function wordWrap(str, maxWidth) {
    var newLineStr = "<br>"; done = false; res = '';
    while (str.length > maxWidth) {                 
        found = false;
        // Inserts new line at first whitespace of the line
        for (i = maxWidth - 1; i >= 0; i--) {
            if (testWhite(str.charAt(i))) {
                res = res + [str.slice(0, i), newLineStr].join('');
                str = str.slice(i + 1);
                found = true;
                break;
            }
        }
        // Inserts new line at maxWidth position, the word is too long to wrap
        if (!found) {
            res += [str.slice(0, maxWidth), newLineStr].join('');
            str = str.slice(maxWidth);
        }

    }
    
    return res + str;
}


function wordWrap2(str, maxWidth) {
    var newLineStr = "\n"; done = false; res = '';
    while (str.length > maxWidth) {                 
        found = false;
        // Inserts new line at first whitespace of the line
        for (i = maxWidth - 1; i >= 0; i--) {
            if (testWhite(str.charAt(i))) {
                res = res + [str.slice(0, i), newLineStr].join('');
                str = str.slice(i + 1);
                found = true;
                break;
            }
        }
        // Inserts new line at maxWidth position, the word is too long to wrap
        if (!found) {
            res += [str.slice(0, maxWidth), newLineStr].join('');
            str = str.slice(maxWidth);
        }

    }
    
    return res + str;
}

function testWhite(x) {
    var white = new RegExp(/^\s$/);
    return white.test(x.charAt(0));
};