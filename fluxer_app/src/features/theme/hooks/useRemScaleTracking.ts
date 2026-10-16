// SPDX-License-Identifier: AGPL-3.0-or-later

import {startRemScaleTracking} from '@app/features/theme/layout/RemFromPx';
import {useLayoutEffect} from 'react';

export function useRemScaleTracking(): void {
	useLayoutEffect(() => startRemScaleTracking(document), []);
}
