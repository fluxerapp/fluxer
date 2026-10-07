// SPDX-License-Identifier: AGPL-3.0-or-later

import {createDesktopModuleRequest} from '@app/features/platform/utils/DesktopModuleRequest';
import {DESKTOP_SOURCEMAP_MODULE_NAME} from '@fluxer/desktop_ipc/src/ModuleContract';

const request = createDesktopModuleRequest(DESKTOP_SOURCEMAP_MODULE_NAME);

export function ensureSourceMapsModule(): Promise<unknown> {
	return request.ensure();
}
