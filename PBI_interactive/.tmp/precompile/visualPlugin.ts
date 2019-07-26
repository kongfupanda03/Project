import { Visual } from "../../src/visual";
var powerbiKey = "powerbi";
var powerbi = window[powerbiKey];

var sampleRAB9781EE44CF4498B9425B2BFCECE4E0_DEBUG = {
    name: 'sampleRAB9781EE44CF4498B9425B2BFCECE4E0_DEBUG',
    displayName: 'sampleR',
    class: 'Visual',
    version: '1.0.0',
    apiVersion: '2.6.0',
    create: (options) => {
        if (Visual) {
            return new Visual(options);
        }

        console.error('Visual instance not found');
    },
    custom: true
};

if (typeof powerbi !== "undefined") {
    powerbi.visuals = powerbi.visuals || {};
    powerbi.visuals.plugins = powerbi.visuals.plugins || {};
    powerbi.visuals.plugins["sampleRAB9781EE44CF4498B9425B2BFCECE4E0_DEBUG"] = sampleRAB9781EE44CF4498B9425B2BFCECE4E0_DEBUG;
}

export default sampleRAB9781EE44CF4498B9425B2BFCECE4E0_DEBUG;