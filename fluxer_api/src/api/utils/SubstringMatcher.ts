// SPDX-License-Identifier: AGPL-3.0-or-later

const ROOT_NODE = 0;
const ASCII_ROOT_SIZE = 128;
const NO_TRANSITION = -1;
const EDGE_KEY_STRIDE = 65536;

export class SubstringMatcher {
	private readonly rowStart: Int32Array;
	private readonly edgeCodes: Int32Array;
	private readonly edgeTargets: Int32Array;
	private readonly failures: Int32Array;
	private readonly terminals: Uint8Array;
	private readonly asciiRoot: Int32Array;
	private readonly matchesEverything: boolean;

	private constructor(
		rowStart: Int32Array,
		edgeCodes: Int32Array,
		edgeTargets: Int32Array,
		failures: Int32Array,
		terminals: Uint8Array,
		matchesEverything: boolean,
	) {
		this.rowStart = rowStart;
		this.edgeCodes = edgeCodes;
		this.edgeTargets = edgeTargets;
		this.failures = failures;
		this.terminals = terminals;
		this.matchesEverything = matchesEverything;
		this.asciiRoot = new Int32Array(ASCII_ROOT_SIZE);
		for (let edge = rowStart[ROOT_NODE]!; edge < rowStart[ROOT_NODE + 1]!; edge++) {
			const code = edgeCodes[edge]!;
			if (code < ASCII_ROOT_SIZE) {
				this.asciiRoot[code] = edgeTargets[edge]!;
			}
		}
	}

	static fromPatterns(patterns: Iterable<string>): SubstringMatcher | null {
		const children = new Map<number, number>();
		const edgeSources: Array<number> = [];
		const edgeLabels: Array<number> = [];
		const edgeChildren: Array<number> = [];
		const terminalNodes: Array<number> = [];
		let nodeCount = 1;
		let matchesEverything = false;
		let count = 0;
		for (const pattern of patterns) {
			count++;
			if (pattern.length === 0) {
				matchesEverything = true;
				continue;
			}
			let node = ROOT_NODE;
			for (let index = 0; index < pattern.length; index++) {
				const code = pattern.charCodeAt(index);
				const key = node * EDGE_KEY_STRIDE + code;
				let next = children.get(key);
				if (next === undefined) {
					next = nodeCount++;
					children.set(key, next);
					edgeSources.push(node);
					edgeLabels.push(code);
					edgeChildren.push(next);
				}
				node = next;
			}
			terminalNodes.push(node);
		}
		if (count === 0) {
			return null;
		}
		children.clear();
		const edgeCount = edgeSources.length;
		const rowStart = new Int32Array(nodeCount + 1);
		for (let edge = 0; edge < edgeCount; edge++) {
			rowStart[edgeSources[edge]! + 1]!++;
		}
		for (let node = 0; node < nodeCount; node++) {
			rowStart[node + 1]! += rowStart[node]!;
		}
		const cursor = rowStart.slice(0, nodeCount);
		const edgeCodes = new Int32Array(edgeCount);
		const edgeTargets = new Int32Array(edgeCount);
		for (let edge = 0; edge < edgeCount; edge++) {
			const source = edgeSources[edge]!;
			const slot = cursor[source]!;
			cursor[source] = slot + 1;
			edgeCodes[slot] = edgeLabels[edge]!;
			edgeTargets[slot] = edgeChildren[edge]!;
		}
		for (let node = 0; node < nodeCount; node++) {
			const start = rowStart[node]!;
			const end = rowStart[node + 1]!;
			for (let index = start + 1; index < end; index++) {
				const code = edgeCodes[index]!;
				const target = edgeTargets[index]!;
				let scan = index - 1;
				while (scan >= start && edgeCodes[scan]! > code) {
					edgeCodes[scan + 1] = edgeCodes[scan]!;
					edgeTargets[scan + 1] = edgeTargets[scan]!;
					scan--;
				}
				edgeCodes[scan + 1] = code;
				edgeTargets[scan + 1] = target;
			}
		}
		const failures = new Int32Array(nodeCount);
		const terminals = new Uint8Array(nodeCount);
		for (const node of terminalNodes) {
			terminals[node] = 1;
		}
		const queue = new Int32Array(nodeCount);
		let tail = 0;
		for (let edge = rowStart[ROOT_NODE]!; edge < rowStart[ROOT_NODE + 1]!; edge++) {
			queue[tail] = edgeTargets[edge]!;
			tail++;
		}
		for (let head = 0; head < tail; head++) {
			const node = queue[head]!;
			if (terminals[failures[node]!]! === 1) {
				terminals[node] = 1;
			}
			for (let edge = rowStart[node]!; edge < rowStart[node + 1]!; edge++) {
				const code = edgeCodes[edge]!;
				const child = edgeTargets[edge]!;
				let candidate = failures[node]!;
				let target = findTransition(rowStart, edgeCodes, edgeTargets, candidate, code);
				while (target === NO_TRANSITION && candidate !== ROOT_NODE) {
					candidate = failures[candidate]!;
					target = findTransition(rowStart, edgeCodes, edgeTargets, candidate, code);
				}
				failures[child] = target === NO_TRANSITION ? ROOT_NODE : target;
				queue[tail] = child;
				tail++;
			}
		}
		return new SubstringMatcher(rowStart, edgeCodes, edgeTargets, failures, terminals, matchesEverything);
	}

	test(text: string): boolean {
		if (text.length === 0) {
			return false;
		}
		if (this.matchesEverything) {
			return true;
		}
		const rowStart = this.rowStart;
		const edgeCodes = this.edgeCodes;
		const edgeTargets = this.edgeTargets;
		const failures = this.failures;
		const terminals = this.terminals;
		const asciiRoot = this.asciiRoot;
		let node = ROOT_NODE;
		for (let index = 0; index < text.length; index++) {
			const code = text.charCodeAt(index);
			if (node === ROOT_NODE && code < ASCII_ROOT_SIZE) {
				node = asciiRoot[code]!;
			} else {
				let next = findTransition(rowStart, edgeCodes, edgeTargets, node, code);
				while (next === NO_TRANSITION && node !== ROOT_NODE) {
					node = failures[node]!;
					next = findTransition(rowStart, edgeCodes, edgeTargets, node, code);
				}
				node = next === NO_TRANSITION ? ROOT_NODE : next;
			}
			if (terminals[node]! === 1) {
				return true;
			}
		}
		return false;
	}
}

function findTransition(
	rowStart: Int32Array,
	edgeCodes: Int32Array,
	edgeTargets: Int32Array,
	node: number,
	code: number,
): number {
	let low = rowStart[node]!;
	let high = rowStart[node + 1]! - 1;
	while (low <= high) {
		const middle = (low + high) >>> 1;
		const candidate = edgeCodes[middle]!;
		if (candidate === code) {
			return edgeTargets[middle]!;
		}
		if (candidate < code) {
			low = middle + 1;
		} else {
			high = middle - 1;
		}
	}
	return NO_TRANSITION;
}
