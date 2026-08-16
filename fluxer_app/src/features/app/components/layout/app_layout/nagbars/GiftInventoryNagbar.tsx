// SPDX-License-Identifier: AGPL-3.0-or-later

import {Nagbar} from '@app/features/app/components/layout/Nagbar';
import {NagbarButton} from '@app/features/app/components/layout/NagbarButton';
import {NagbarContent} from '@app/features/app/components/layout/NagbarContent';
import {NAGBAR_TONES, NagbarToneKind} from '@app/features/app/components/layout/NagbarTones';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as UserCommands from '@app/features/user/commands/UserCommands';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import Users from '@app/features/user/state/Users';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const GIFT_INVENTORY_MESSAGE_DESCRIPTOR = msg({
	message:
		'{count, plural, one {You have a new gift code waiting in your gift inventory.} other {You have # new gift codes waiting in your gift inventory.}}',
	comment: 'Nagbar body shown when the user has unread gift inventory items. {count} is the unread gift count.',
});
const VIEW_GIFT_INVENTORY_DESCRIPTOR = msg({
	message: 'View gift inventory',
	comment: 'Button label on the gift-inventory nagbar. Opens the gift inventory settings tab.',
});
const GiftInventoryNagbarContent = observer(function GiftInventoryNagbarContent({isMobile}: {isMobile: boolean}) {
	const {i18n} = useLingui();
	const currentUser = Users.currentUser;
	const unreadCount = currentUser?.unreadGiftInventoryCount ?? 1;
	const message = i18n._(GIFT_INVENTORY_MESSAGE_DESCRIPTOR, {count: unreadCount});
	const handleOpenGiftInventory = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<UserSettingsModal
					initialTab="gift_inventory"
					data-flx="app.app-layout.nagbars.gift-inventory-nagbar.handle-open-gift-inventory.user-settings-modal"
				/>
			)),
		);
	}, []);
	const handleDismiss = useCallback(() => {
		UserCommands.update({has_unread_gift_inventory: false});
	}, []);
	return (
		<Nagbar
			isMobile={isMobile}
			backgroundColor={NAGBAR_TONES[NagbarToneKind.BRAND].backgroundColor}
			textColor={NAGBAR_TONES[NagbarToneKind.BRAND].textColor}
			dismissible
			onDismiss={handleDismiss}
			data-flx="app.app-layout.nagbars.gift-inventory-nagbar.nagbar"
		>
			<NagbarContent
				isMobile={isMobile}
				onDismiss={handleDismiss}
				message={message}
				actions={
					<NagbarButton
						isMobile={isMobile}
						onClick={handleOpenGiftInventory}
						data-flx="app.app-layout.nagbars.gift-inventory-nagbar.nagbar-button.open-gift-inventory"
					>
						{i18n._(VIEW_GIFT_INVENTORY_DESCRIPTOR)}
					</NagbarButton>
				}
				data-flx="app.app-layout.nagbars.gift-inventory-nagbar.nagbar-content"
			/>
		</Nagbar>
	);
});
export const GiftInventoryNagbar = observer(({isMobile}: {isMobile: boolean}) => {
	if (!shouldShowPremiumFeatures()) {
		return null;
	}
	return (
		<GiftInventoryNagbarContent
			isMobile={isMobile}
			data-flx="app.app-layout.nagbars.gift-inventory-nagbar.gift-inventory-nagbar-content"
		/>
	);
});
