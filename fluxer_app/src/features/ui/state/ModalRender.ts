// SPDX-License-Identifier: AGPL-3.0-or-later

import type React from 'react';

export type ModalType = 'user-settings' | 'guild-settings' | 'channel-settings';

export interface ModalRender {
	(): React.ReactElement;
	modalType?: ModalType;
}
