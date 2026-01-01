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

import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {Trans} from '@lingui/react/macro';
import * as AccessibilityActionCreators from '~/actions/AccessibilityActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {ConfirmModal} from '~/components/modals/ConfirmModal';

export const confirmHideFavorites = (onConfirm: (() => void) | undefined, i18n: I18n): void => {
	ModalActionCreators.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(msg`Hide Favorites`)}
				description={
					<div>
						<Trans>
							This will hide all favorites-related UI elements including buttons and menu items. Your existing favorites
							will be preserved and can be re-enabled anytime from{' '}
							<strong>User Settings → Look & Feel → Favorites</strong>.
						</Trans>
					</div>
				}
				primaryText={i18n._(msg`Hide Favorites`)}
				primaryVariant="danger-primary"
				onPrimary={() => {
					AccessibilityActionCreators.update({showFavorites: false});
					onConfirm?.();
				}}
			/>
		)),
	);
};
