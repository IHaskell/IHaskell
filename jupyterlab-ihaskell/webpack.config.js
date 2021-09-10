const fs = require('fs-extra');
const path = require('path');

const packagePath = '.';
const data = fs.readJSONSync(path.join(packagePath, 'package.json'));
const outputPath = path.join(packagePath, data.jupyterlab['outputDir']);

class FixupEntryPoint {
  apply(compiler) {
    compiler.hooks.done.tap('FixupEntryPoint', (stats) => {
      const data = fs.readJSONSync(path.join(outputPath, "package.json"));
      const remoteEntry = data.jupyterlab._build.load;
      const remoteEntryRe = /static\/remoteEntry\.(.*)\.js/;
      const remoteEntryDash = remoteEntry.replace(remoteEntryRe, "static/remoteEntry-$1.js");
      fs.moveSync(path.join(outputPath, remoteEntry), path.join(outputPath, remoteEntryDash));
      data.jupyterlab._build.load = remoteEntryDash;
      fs.writeJSONSync(path.join(outputPath, "package.json"), data, { spaces: 2 });
    });
  };
};

module.exports = {
  output: {
    filename: "[name]-[contenthash].js"
  },
  plugins: [
    new FixupEntryPoint()
  ]
}
