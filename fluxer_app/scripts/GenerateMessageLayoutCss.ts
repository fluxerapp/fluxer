// SPDX-License-Identifier: AGPL-3.0-or-later

import {existsSync, mkdirSync, readFileSync, writeFileSync} from 'node:fs';
import {dirname, join, relative} from 'node:path';
import {renderMessageLayoutCss} from '@app/features/theme/layout/MessageLayoutCss';

function main(): void {
	const scriptDir = import.meta.dirname;
	const appDir = join(scriptDir, '..');
	const cssPath = join(appDir, 'src', 'features', 'theme', 'styles', 'generated', 'message-layout.css');
	const css = renderMessageLayoutCss();
	if (process.argv.includes('--check')) {
		if (!existsSync(cssPath) || readFileSync(cssPath, 'utf8') !== css) {
			throw new Error(`${relative(appDir, cssPath)} is stale. Run pnpm generate:message-layout.`);
		}
		console.log(`Checked ${relative(appDir, cssPath)}`);
		return;
	}
	mkdirSync(dirname(cssPath), {recursive: true});
	writeFileSync(cssPath, css);
	console.log(`Wrote ${relative(appDir, cssPath)}`);
}

main();
