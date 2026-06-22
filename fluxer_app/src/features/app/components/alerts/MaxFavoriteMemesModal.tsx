// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {Limits} from '@app/features/app/utils/UserLimits';
import {CLOSE_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as PremiumModalCommands from '@app/features/premium/commands/PremiumModalCommands';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import Users from '@app/features/user/state/Users';
import {MAX_FAVORITE_MEMES_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {msg, plural} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {GenericErrorModal} from './GenericErrorModal';

const SAVED_MEDIA_LIMIT_REACHED_DESCRIPTOR = msg({
	message: 'Saved media limit reached',
	comment: 'Short label in the max favorite memes modal.',
});
const YOU_VE_REACHED_THE_MAXIMUM_LIMIT_OF_THIS_DESCRIPTOR = msg({
	message: "You're at the cap of {freeItemsText} — your instance admin sets this limit.",
	comment:
		'Modal body shown when the favorite-memes limit is reached on a self-hosted instance with no Plutonium upgrade path. Limit text is interpolated.',
});
const YOU_VE_REACHED_THE_MAXIMUM_LIMIT_OF_FOR_DESCRIPTOR = msg({
	message: "You're at the free cap of {freeItemsText}. {premiumProductName} bumps you up to {premiumItemsText}.",
	comment:
		'Modal body shown when the free favorite-memes limit is reached and Plutonium would increase it. Free and Plutonium limits are interpolated.',
});
const UPGRADE_TO_DESCRIPTOR = msg({
	message: 'Upgrade to {premiumProductName}',
	comment: 'Short label in the max favorite memes modal. Preserve {premiumProductName}; it is inserted by code.',
});
const MAYBE_LATER_DESCRIPTOR = msg({
	message: 'Maybe later',
	comment: 'Short label in the max favorite memes modal.',
});
export const MaxFavoriteMemesModal = observer(() => {
	const {i18n} = useLingui();
	const currentUser = Users.currentUser;
	const showPremium = shouldShowPremiumFeatures();
	const premiumLimit = Limits.getPremiumValue('max_favorite_memes', MAX_FAVORITE_MEMES_PREMIUM);
	const maxFavoriteMemes = currentUser?.maxFavoriteMemes ?? premiumLimit;
	const canUpgradeFavoriteMemes = maxFavoriteMemes < premiumLimit;
	const freeItemsText = plural(
		{count: maxFavoriteMemes},
		{
			one: '# saved media item',
			other: '# saved media items',
		},
	);
	if (!showPremium) {
		return (
			<GenericErrorModal
				title={i18n._(SAVED_MEDIA_LIMIT_REACHED_DESCRIPTOR)}
				message={i18n._(YOU_VE_REACHED_THE_MAXIMUM_LIMIT_OF_THIS_DESCRIPTOR, {freeItemsText})}
				data-flx="app.max-favorite-memes-modal.confirm-modal"
			/>
		);
	}
	if (!canUpgradeFavoriteMemes) {
		const description = plural(
			{count: maxFavoriteMemes},
			{
				one: "You're at the cap of # saved media item. Remove one to add another.",
				other: "You're at the cap of # saved media items. Remove one to add another.",
			},
		);
		return (
			<ConfirmModal
				title={i18n._(SAVED_MEDIA_LIMIT_REACHED_DESCRIPTOR)}
				description={description}
				secondaryText={i18n._(CLOSE_DESCRIPTOR)}
				hideCloseButton
				data-flx="app.max-favorite-memes-modal.confirm-modal--2"
			/>
		);
	}
	const premiumItemsText = plural(
		{count: premiumLimit},
		{
			one: '# saved media item',
			other: '# saved media items',
		},
	);
	const freeDescription = i18n._(YOU_VE_REACHED_THE_MAXIMUM_LIMIT_OF_FOR_DESCRIPTOR, {
		freeItemsText,
		premiumProductName: PREMIUM_PRODUCT_NAME,
		premiumItemsText,
	});
	return (
		<ConfirmModal
			title={i18n._(SAVED_MEDIA_LIMIT_REACHED_DESCRIPTOR)}
			description={freeDescription}
			primaryText={i18n._(UPGRADE_TO_DESCRIPTOR, {premiumProductName: PREMIUM_PRODUCT_NAME})}
			primaryVariant="primary"
			onPrimary={() => {
				window.setTimeout(() => {
					PremiumModalCommands.open();
				}, 0);
			}}
			secondaryText={i18n._(MAYBE_LATER_DESCRIPTOR)}
			data-flx="app.max-favorite-memes-modal.confirm-modal--3"
		/>
	);
});
