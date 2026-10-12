// SPDX-License-Identifier: AGPL-3.0-or-later

import {readdirSync, readFileSync} from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {describe, expect, test} from 'vitest';

const REPO_ROOT = fileURLToPath(new URL('../../../../', import.meta.url));
const CHART = path.join(REPO_ROOT, 'deploy/helm/fluxer');
const CONFIGURATION_PAGE = path.join(REPO_ROOT, 'fluxer_docs/src/content/docs/operator/configuration.mdx');

const fixedWiringNames = (): Array<string> => {
	const page = readFileSync(CONFIGURATION_PAGE, 'utf8');
	const start = page.indexOf('#### Fixed wiring');
	const end = page.indexOf('\n#### ', start + 1);
	const section = page.slice(start, end);
	return [...new Set([...section.matchAll(/`([A-Z][A-Z0-9_]*)`/g)].map(([, name]) => name))].sort();
};

const chartTemplates = (): string => {
	const directory = path.join(CHART, 'templates');
	return readdirSync(directory)
		.filter((name) => name.endsWith('.yaml') || name.endsWith('.tpl'))
		.map((name) => readFileSync(path.join(directory, name), 'utf8'))
		.join('\n');
};

describe('the Helm chart matches the Compose stack', () => {
	test('the chart ships the same Caddyfile as Compose', () => {
		const compose = readFileSync(path.join(REPO_ROOT, 'deploy/self-hosting/Caddyfile'));
		const chart = readFileSync(path.join(CHART, 'files/Caddyfile'));
		expect(chart.equals(compose)).toBe(true);
	});

	test('the chart sets every name Compose pins as fixed wiring', () => {
		const names = fixedWiringNames();
		expect(names.length).toBeGreaterThan(10);
		const templates = chartTemplates();
		const missing = names.filter((name) => !templates.includes(`"${name}"`));
		expect(missing).toEqual([]);
	});
});
