// SPDX-License-Identifier: AGPL-3.0-or-later

const PROCESS_SIGNATURE =
	'process(e,t,n){return e.length===0||!e[0]||e[0]?.length===0||!this.processor||this.processor.process(e[0],t[0]),!0}';

const PASSTHROUGH_PROCESS =
	'process(e,t,n){' +
	'const __in=e[0],__out=t[0];' +
	'if(!__in||__in.length===0||!__out)return!0;' +
	'if(!this.processor){' +
	'for(let __ch=0;__ch<__out.length;__ch++){' +
	'const __src=__in[Math.min(__ch,__in.length-1)];' +
	'if(__src&&__out[__ch])__out[__ch].set(__src)' +
	'}' +
	'return!0' +
	'}' +
	'return this.processor.process(__in,__out),!0' +
	'}';

const PORT_LISTENER = 'this.port.addEventListener(`message`,e=>{e.data===`destroy`&&this.destroy()})';
const PORT_LISTENER_STARTED = `${PORT_LISTENER},this.port.start()`;

const READY_MARKER = 'this.destroyed&&this.destroy()})()}';
const READY_MARKER_SIGNALLED =
	'this.destroyed&&this.destroy(),this.destroyed||this.port.postMessage({type:"ready"})})()' +
	'.catch(__err=>{this.port.postMessage({type:"error",message:String(__err)})})}';

const GATE_CONSTRUCTOR_TAIL = 'this.processor=i(e.processorOptions,t)}';
const GATE_CONSTRUCTOR_TAIL_SIGNALLED = 'this.processor=i(e.processorOptions,t),this.port.postMessage({type:"ready"})}';

function replaceExactlyOnce(source, needle, replacement, resourcePath) {
	const occurrences = source.split(needle).length - 1;
	if (occurrences !== 1) {
		throw new Error(
			`noise-suppressor-worklet-loader: expected exactly one occurrence of the upstream snippet in ${resourcePath}, found ${occurrences}. The @sapphi-red/web-noise-suppressor build changed shape; update this loader.`,
		);
	}
	return source.replace(needle, replacement);
}

module.exports = function noiseSuppressorWorkletLoader(source) {
	const resourcePath = this.resourcePath;
	if (resourcePath.includes('noiseGate')) {
		return replaceExactlyOnce(source, GATE_CONSTRUCTOR_TAIL, GATE_CONSTRUCTOR_TAIL_SIGNALLED, resourcePath);
	}
	let output = replaceExactlyOnce(source, PORT_LISTENER, PORT_LISTENER_STARTED, resourcePath);
	output = replaceExactlyOnce(output, READY_MARKER, READY_MARKER_SIGNALLED, resourcePath);
	return replaceExactlyOnce(output, PROCESS_SIGNATURE, PASSTHROUGH_PROCESS, resourcePath);
};
