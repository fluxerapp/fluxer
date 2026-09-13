// SPDX-License-Identifier: AGPL-3.0-or-later
/// <reference types="node" />

import {readFileSync} from 'node:fs';
import path from 'node:path';
import {fileURLToPath} from 'node:url';
import * as implementationsBarrel from '@fluxer/voice_engine_v2/src/implementations';
import * as packageBarrel from '@fluxer/voice_engine_v2/src/index';
import {describe, expect, it} from 'vitest';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const PACKAGE_ROOT = path.resolve(__dirname, '..');
const PUBLIC_EXPORT_FILES = ['index.ts', 'implementations/index.ts'];
const QUARANTINED_TESTING_EXPORTS = ['VoiceEngineV2TestImplementation', 'VoiceEngineV2TestDriver'];
const LEGACY_TESTING_EXPORTS = ['JsVoiceEngineV2Implementation', 'VoiceEngineV2JsDriver'];
const TESTING_MODULE_REFERENCE_PATTERN = /from\s+['"](?:[./]*|@fluxer\/voice_engine_v2\/src\/)testing(?:\/[^'"]*)?['"]/;

const PUBLIC_BARRELS: ReadonlyArray<[string, Record<string, unknown>]> = [
	['src/index.ts', packageBarrel],
	['src/implementations/index.ts', implementationsBarrel],
];

function readPublicExportSource(relativePath: string): string {
	return readFileSync(path.join(PACKAGE_ROOT, relativePath), 'utf8');
}

describe('voice engine v2 implementation export boundary', () => {
	it('does not expose the testing substrate from resolved public barrel exports', () => {
		for (const [label, barrel] of PUBLIC_BARRELS) {
			const exportNames = Object.keys(barrel);
			expect(exportNames.length, `${label} must have runtime exports`).toBeGreaterThan(0);
			for (const exportName of QUARANTINED_TESTING_EXPORTS) {
				expect(exportNames, `${label} must not export ${exportName}`).not.toContain(exportName);
			}
		}
	});

	it('does not reference the testing directory from public barrel sources', () => {
		for (const relativePath of PUBLIC_EXPORT_FILES) {
			const source = readPublicExportSource(relativePath);
			expect(source, `${relativePath} must not re-export from ./testing`).not.toMatch(TESTING_MODULE_REFERENCE_PATTERN);
		}
	});

	it('does not mention quarantined or legacy testing identifiers in public barrel sources', () => {
		for (const relativePath of PUBLIC_EXPORT_FILES) {
			const source = readPublicExportSource(relativePath);
			for (const exportName of [...QUARANTINED_TESTING_EXPORTS, ...LEGACY_TESTING_EXPORTS]) {
				expect(source, `${relativePath} must not mention ${exportName}`).not.toContain(exportName);
			}
		}
	});
});
