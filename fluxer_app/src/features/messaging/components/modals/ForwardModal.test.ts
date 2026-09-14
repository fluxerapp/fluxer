// SPDX-License-Identifier: AGPL-3.0-or-later

import {shouldNavigateAfterForward} from '@app/features/messaging/components/modals/ForwardModalUtils';
import {describe, expect, it} from 'vitest';

describe('ForwardModal', () => {
	it('navigates only for an unmodified single-destination forward', () => {
		expect(shouldNavigateAfterForward(false, 1)).toBe(true);
		expect(shouldNavigateAfterForward(true, 1)).toBe(false);
		expect(shouldNavigateAfterForward(false, 2)).toBe(false);
		expect(shouldNavigateAfterForward(false, 0)).toBe(false);
	});
});
