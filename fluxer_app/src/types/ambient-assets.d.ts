// SPDX-License-Identifier: AGPL-3.0-or-later

declare module '*.css';

declare module '@arborium/*/grammar.js' {
	const grammar: unknown;
	export default grammar;
}

declare module '@arborium/*/grammar_bg.wasm' {
	const url: string;
	export default url;
}

declare module '@arborium/arborium/arborium_host_bg.wasm' {
	const url: string;
	export default url;
}
