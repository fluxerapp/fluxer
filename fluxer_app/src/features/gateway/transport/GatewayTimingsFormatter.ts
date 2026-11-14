// SPDX-License-Identifier: AGPL-3.0-or-later

interface RpcTimingStep {
	duration_us: number;
	steps?: Record<string, RpcTimingStep>;
}

export interface RpcTimings {
	node_name?: string;
	pod_name: string;
	role?: string;
	steps: Record<string, RpcTimingStep>;
	total_us: number;
	unit?: string;
}

interface GatewayTraceNode {
	name: string;
	duration_us: number;
	children?: Array<GatewayTraceNode>;
	remote?: {
		operation: string;
		pod_name: string;
	};
}

export interface GatewayTimings {
	pod_name: string;
	total_us: number;
	trace: Array<GatewayTraceNode>;
	unit?: string;
}

const INDENT = '|  ';

function formatMs(durationUs: number): string {
	return String(Number((durationUs / 1000).toFixed(3)));
}

function pushLine(lines: Array<string>, label: string, durationUs: number, depth: number): void {
	lines.push(`${INDENT.repeat(depth)}${label}: ${formatMs(durationUs)}`);
}

function appendRpcSteps(lines: Array<string>, steps: Record<string, RpcTimingStep>, depth: number): void {
	for (const [name, step] of Object.entries(steps)) {
		pushLine(lines, name, step.duration_us, depth);
		if (step.steps) {
			appendRpcSteps(lines, step.steps, depth + 1);
		}
	}
}

function appendRpcTree(lines: Array<string>, rpc: RpcTimings, depth: number): void {
	const label = rpc.role ? `${rpc.pod_name} (${rpc.role})` : rpc.pod_name;
	pushLine(lines, label, rpc.total_us, depth);
	appendRpcSteps(lines, rpc.steps, depth + 1);
}

interface GraftContext {
	rpc?: RpcTimings;
	grafted: boolean;
}

interface Leaf {
	name: string;
	durationUs: number;
}

function collectRpcLeaves(steps: Record<string, RpcTimingStep>, leaves: Array<Leaf>): void {
	for (const [name, step] of Object.entries(steps)) {
		if (step.steps) {
			collectRpcLeaves(step.steps, leaves);
		} else {
			leaves.push({name, durationUs: step.duration_us});
		}
	}
}

function collectTraceLeaves(nodes: Array<GatewayTraceNode>, leaves: Array<Leaf>, rpc?: RpcTimings): void {
	for (const node of nodes) {
		if (rpc && isRpcGraftPoint(node, rpc)) continue;
		if (node.children && node.children.length > 0) {
			collectTraceLeaves(node.children, leaves, rpc);
		} else {
			leaves.push({name: node.name, durationUs: node.duration_us});
		}
	}
}

const SLOWEST_LEAF_COUNT = 3;

function buildSlowestSummary(gw?: GatewayTimings, rpc?: RpcTimings): string | null {
	const leaves: Array<Leaf> = [];
	if (gw) collectTraceLeaves(gw.trace, leaves, rpc);
	if (rpc) collectRpcLeaves(rpc.steps, leaves);
	if (leaves.length === 0) return null;
	const top = leaves
		.sort((a, b) => b.durationUs - a.durationUs)
		.slice(0, SLOWEST_LEAF_COUNT)
		.map((leaf) => `${leaf.name} (${formatMs(leaf.durationUs)})`);
	return `slowest: ${top.join(', ')}`;
}

function isRpcGraftPoint(node: GatewayTraceNode, rpc: RpcTimings): boolean {
	return node.remote?.operation === 'api' && node.remote.pod_name === rpc.pod_name;
}

function appendTrace(lines: Array<string>, nodes: Array<GatewayTraceNode>, depth: number, ctx: GraftContext): void {
	for (const node of nodes) {
		const label = node.remote ? `${node.name} -> ${node.remote.pod_name}` : node.name;
		pushLine(lines, label, node.duration_us, depth);
		if (node.children) {
			appendTrace(lines, node.children, depth + 1, ctx);
		}
		if (ctx.rpc && !ctx.grafted && isRpcGraftPoint(node, ctx.rpc)) {
			ctx.grafted = true;
			appendRpcTree(lines, ctx.rpc, depth + 1);
		}
	}
}

export function formatGatewayReadyTimings(gw?: GatewayTimings, rpc?: RpcTimings): string | null {
	if (!gw && !rpc) return null;
	const lines: Array<string> = [];
	const summary = buildSlowestSummary(gw, rpc);
	if (summary) {
		lines.push(summary);
	}
	if (gw) {
		pushLine(lines, gw.pod_name, gw.total_us, 0);
		const ctx: GraftContext = {rpc, grafted: false};
		appendTrace(lines, gw.trace, 1, ctx);
		if (rpc && !ctx.grafted) {
			appendRpcTree(lines, rpc, 1);
		}
	} else if (rpc) {
		appendRpcTree(lines, rpc, 0);
	}
	return lines.join('\n');
}
