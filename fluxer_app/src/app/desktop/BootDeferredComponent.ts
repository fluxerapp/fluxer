// SPDX-License-Identifier: AGPL-3.0-or-later

import {whenBootRevealed} from '@app/features/boot_state/BootReveal';
import {createNamedLoadableComponent} from '@app/features/platform/components/loadable/LoadableComponent';
import type {ComponentType} from 'react';

export function createBootDeferredComponent<Props extends object>(
	displayName: string,
	load: () => Promise<ComponentType<Props>>,
) {
	const Component = createNamedLoadableComponent<Props>({
		displayName,
		load: async () => {
			await whenBootRevealed();
			return load();
		},
	});
	return Component;
}
