// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {Limits} from '@app/features/app/utils/UserLimits';
import {CLOSE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import type {User} from '@app/features/user/models/User';
import {MAX_BOOKMARKS_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const BOOKMARK_LIMIT_REACHED_DESCRIPTOR = msg({
	message: 'Bookmark limit reached',
	comment: 'Short label in the max bookmarks modal.',
});
const YOU_VE_REACHED_THE_MAXIMUM_NUMBER_OF_BOOKMARKS_DESCRIPTOR = msg({
	message:
		"You're at {count, plural, one {# bookmark} other {# bookmarks}} — your instance admin sets this cap. Remove one to add another.",
	comment:
		'Modal body shown when the bookmarks limit is reached on a self-hosted instance with no upgrade path. {count} is the bookmark cap.',
});
const YOU_VE_REACHED_THE_MAXIMUM_NUMBER_OF_BOOKMARKS_2_DESCRIPTOR = msg({
	message: "You're at {count, plural, one {# bookmark} other {# bookmarks}}. Remove one to add another.",
	comment:
		'Modal body shown when the bookmarks limit is reached and Plutonium is unavailable on this instance. {count} is the bookmark cap.',
});
const YOU_VE_REACHED_THE_MAXIMUM_NUMBER_OF_BOOKMARKS_3_DESCRIPTOR = msg({
	message:
		"You're at the free cap of {count, plural, one {# bookmark} other {# bookmarks}}. {premiumProductName} bumps you up to {premiumCount, plural, one {# bookmark} other {# bookmarks}} — or remove one to add another.",
	comment:
		'Modal body shown when the free bookmarks limit is reached and Plutonium would increase it. {count} is the free cap, {premiumCount} the Plutonium cap.',
});
const UPGRADE_TO_DESCRIPTOR = msg({
	message: 'Upgrade to {premiumProductName}',
	comment: 'Short label in the max bookmarks modal. Preserve {premiumProductName}; it is inserted by code.',
});

interface MaxBookmarksModalProps {
	user: User;
}

export const MaxBookmarksModal = observer(({user}: MaxBookmarksModalProps) => {
	const {i18n} = useLingui();
	const showPremium = shouldShowPremiumFeatures();
	const maxBookmarks = user.maxBookmarks;
	const premiumBookmarks = Limits.getPremiumValue('max_bookmarks', MAX_BOOKMARKS_PREMIUM);
	const canUpgradeBookmarks = maxBookmarks < premiumBookmarks;
	if (!showPremium) {
		return (
			<GenericErrorModal
				title={i18n._(BOOKMARK_LIMIT_REACHED_DESCRIPTOR)}
				message={i18n._(YOU_VE_REACHED_THE_MAXIMUM_NUMBER_OF_BOOKMARKS_DESCRIPTOR, {count: maxBookmarks})}
				data-flx="app.max-bookmarks-modal.confirm-modal"
			/>
		);
	}
	if (!canUpgradeBookmarks) {
		return (
			<GenericErrorModal
				title={i18n._(BOOKMARK_LIMIT_REACHED_DESCRIPTOR)}
				message={i18n._(YOU_VE_REACHED_THE_MAXIMUM_NUMBER_OF_BOOKMARKS_2_DESCRIPTOR, {count: maxBookmarks})}
				data-flx="app.max-bookmarks-modal.confirm-modal--2"
			/>
		);
	}
	return (
		<ConfirmModal
			title={i18n._(BOOKMARK_LIMIT_REACHED_DESCRIPTOR)}
			description={i18n._(YOU_VE_REACHED_THE_MAXIMUM_NUMBER_OF_BOOKMARKS_3_DESCRIPTOR, {
				count: maxBookmarks,
				premiumProductName: PREMIUM_PRODUCT_NAME,
				premiumCount: premiumBookmarks,
			})}
			primaryText={i18n._(UPGRADE_TO_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
			primaryVariant="primary"
			onPrimary={() => {
				window.setTimeout(() => {
					PremiumModalCommands.open();
				}, 0);
			}}
			secondaryText={i18n._(CLOSE_DESCRIPTOR)}
			data-flx="app.max-bookmarks-modal.confirm-modal--3"
		/>
	);
});
