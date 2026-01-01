/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import * as AccessibilityActionCreators from '~/actions/AccessibilityActionCreators';
import {Switch} from '~/components/form/Switch';
import AccessibilityStore from '~/stores/AccessibilityStore';

export const KeyboardTabContent: React.FC = observer(() => {
	const {t} = useLingui();
	const showTextareaFocusRing = AccessibilityStore.showTextareaFocusRing;
	const escapeExitsKeyboardMode = AccessibilityStore.escapeExitsKeyboardMode;

	return (
		<>
			<Switch
				label={t`Show focus ring on chat textarea`}
				description={t`Display a visible focus indicator around the message input when focused. Disable for a more subtle appearance.`}
				value={showTextareaFocusRing}
				onChange={(value) => AccessibilityActionCreators.update({showTextareaFocusRing: value})}
			/>

			<Switch
				label={t`Escape key exits keyboard mode`}
				description={t`Allow pressing Escape to exit keyboard navigation mode. Note: This may conflict with other uses of Escape.`}
				value={escapeExitsKeyboardMode}
				onChange={(value) => AccessibilityActionCreators.update({escapeExitsKeyboardMode: value})}
			/>
		</>
	);
});
