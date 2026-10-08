// SPDX-License-Identifier: AGPL-3.0-or-later

import path from 'node:path';
import {app} from 'electron';

export function getDesktopDistributionPath(...segments: ReadonlyArray<string>): string {
	return path.join(app.getAppPath(), 'dist', ...segments);
}
