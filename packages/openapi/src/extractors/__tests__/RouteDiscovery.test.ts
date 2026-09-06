// SPDX-License-Identifier: AGPL-3.0-or-later

import path from 'node:path';
import {fileURLToPath} from 'node:url';
import {discoverControllerFiles, extractRoutesFromControllers} from '@fluxer/openapi/src/extractors/RouteExtractor';
import {beforeAll, describe, expect, it} from 'vitest';

const API_PACKAGE_PATH = path.join(fileURLToPath(new URL('../../../../../', import.meta.url)), 'fluxer_api');

describe('discoverControllerFiles', () => {
	let files: Array<string>;
	let shapes: Set<string>;
	beforeAll(() => {
		files = discoverControllerFiles(API_PACKAGE_PATH);
		shapes = new Set(extractRoutesFromControllers(files).map((route) => `${route.method.toUpperCase()} ${route.path}`));
	});
	it('leaves test sources out of the discovered set', () => {
		expect(files.filter((file) => file.endsWith('.test.ts'))).toEqual([]);
		expect(files.filter((file) => file.includes('/tests/'))).toEqual([]);
	});
	it('reads routes registered in a *Controller.ts file', () => {
		expect(shapes).toContain('GET /gifs/search');
		expect(shapes).toContain('GET /gifs/featured');
	});
	it('reads routes registered outside a *Controller.ts file', () => {
		expect(shapes).toContain('POST /webhooks/twilio/sms');
		expect(shapes).toContain('GET /_metrics');
		expect(shapes).toContain('GET /_health');
	});
});
