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
import {WarningCircleIcon} from '@phosphor-icons/react';
import previewStyles from '~/components/shared/MessagePreview.module.css';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';

interface SavedMessageMissingCardProps {
	entryId: string;
	onRemove: () => void;
}

export const SavedMessageMissingCard = ({entryId, onRemove}: SavedMessageMissingCardProps) => {
	const {t} = useLingui();
	return (
		<div key={`lost-${entryId}`} className={previewStyles.previewCard}>
			<div className={previewStyles.lostMessageInner}>
				<WarningCircleIcon className={previewStyles.lostMessageIcon} weight="duotone" />
				<p className={previewStyles.lostMessageText}>{t`You lost access to this saved message. Remove?`}</p>
			</div>

			<div className={previewStyles.actionButtons}>
				<FocusRing offset={-2}>
					<button type="button" className={previewStyles.actionButton} onClick={onRemove}>
						{t`Remove`}
					</button>
				</FocusRing>
			</div>
		</div>
	);
};
