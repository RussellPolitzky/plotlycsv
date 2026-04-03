const { ChromoteSession } = require('chromote');

async function test() {
  const b = await ChromoteSession.new();
  await b.Page.navigate('file:///' + process.cwd().replace(/\\/g, '/') + '/repro.html');
  await new Promise(r => setTimeout(r, 3000));
  const res = await b.Runtime.evaluate({ expression: '!!document.getElementById("plot1")._tracesToCSV', returnByValue: true });
  console.log('Decorated:', res.result.value);
  await b.close();
}

test().catch(console.error);
