// SPDX-License-Identifier: AGPL-3.0-or-later

import {mkdirSync, writeFileSync} from 'node:fs';
import {dirname, join, relative} from 'node:path';
import {renderMessageLayoutCss} from '@app/features/theme/layout/MessageLayoutCss';

function main(): void {
	const scriptDir = import.meta.dirname;
	const appDir = join(scriptDir, '..');
	const cssPath = join(appDir, 'src', 'features', 'theme', 'styles', 'generated', 'message-layout.css');
	const css = renderMessageLayoutCss();
	mkdirSync(dirname(cssPath), {recursive: true});
	writeFileSync(cssPath, css);
	console.log(`Wrote ${relative(appDir, cssPath)}`);
}

main();
