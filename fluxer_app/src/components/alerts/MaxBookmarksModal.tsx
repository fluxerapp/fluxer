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

import {modal, push} from '@app/actions/ModalActionCreators';
import {ConfirmModal} from '@app/components/modals/ConfirmModal';
import {PremiumModal} from '@app/components/modals/PremiumModal';
import type {UserRecord} from '@app/records/UserRecord';
import {Limits} from '@app/utils/limits/UserLimits';
import {shouldShowPremiumFeatures} from '@app/utils/PremiumUtils';
import {MAX_BOOKMARKS_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

interface MaxBookmarksModalProps {
	user: UserRecord;
}

export const MaxBookmarksModal = observer(({user}: MaxBookmarksModalProps) => {
	const {t} = useLingui();
	const showPremium = shouldShowPremiumFeatures();
	const maxBookmarks = user.maxBookmarks;
	const premiumBookmarks = Limits.getPremiumValue('max_bookmarks', MAX_BOOKMARKS_PREMIUM);
	const canUpgradeBookmarks = maxBookmarks < premiumBookmarks;

	const bookmarksText = maxBookmarks === 1 ? t`${maxBookmarks} bookmark` : t`${maxBookmarks} bookmarks`;

	if (!showPremium) {
		return (
			<ConfirmModal
				title={t`Bookmark Limit Reached`}
				description={t`You've reached the maximum number of bookmarks (${bookmarksText}). This limit is configured by your instance administrator. Please remove some bookmarks before adding new ones.`}
				primaryText={t`Understood`}
				onPrimary={() => {}}
			/>
		);
	}

	if (!canUpgradeBookmarks) {
		return (
			<ConfirmModal
				title={t`Bookmark Limit Reached`}
				description={t`You've reached the maximum number of bookmarks (${bookmarksText}). Please remove some bookmarks before adding new ones.`}
				primaryText={t`Understood`}
				onPrimary={() => {}}
			/>
		);
	}

	const premiumBookmarksText =
		premiumBookmarks === 1 ? t`${premiumBookmarks} bookmark` : t`${premiumBookmarks} bookmarks`;

	return (
		<ConfirmModal
			title={t`Bookmark Limit Reached`}
			description={t`You've reached the maximum number of bookmarks for free users (${bookmarksText}). Upgrade to Plutonium to increase your limit to ${premiumBookmarksText}, or remove some bookmarks to add new ones.`}
			primaryText={t`Upgrade to Plutonium`}
			primaryVariant="primary"
			onPrimary={() => {
				window.setTimeout(() => {
					push(modal(() => <PremiumModal />));
				}, 0);
			}}
			secondaryText={t`Dismiss`}
		/>
	);
});
