// SPDX-License-Identifier: AGPL-3.0-or-later

import {Switch} from '@app/features/ui/components/form/FormSwitch';
import AdvancedSettings from '@app/features/user/state/AdvancedSettings';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';

const ENABLE_UNREAD_BADGE_CUSTOMIZATION_DESCRIPTOR = msg({
	message: 'Enable unread badge customization',
	comment: 'Short label for the experimental advanced setting.',
});

export const UnreadBadgeCustomizationControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(ENABLE_UNREAD_BADGE_CUSTOMIZATION_DESCRIPTOR)}
			value={AdvancedSettings.unreadBadgeCustomizationEnabled}
			onChange={AdvancedSettings.setUnreadBadgeCustomizationEnabled}
			compact
			data-flx="user.advanced-settings-tab.switch.unread-badge-customization"
		/>
	);
});

const ENABLE_EXPRESSION_CLONE_SHORTCUTS_DESCRIPTOR = msg({
	message: 'Enable emoji and sticker cloning shortcuts',
	comment: 'Short label for the experimental advanced setting.',
});

export const ExpressionCloneShortcutsControl = observer(() => {
	const {i18n} = useLingui();
	return (
		<Switch
			ariaLabel={i18n._(ENABLE_EXPRESSION_CLONE_SHORTCUTS_DESCRIPTOR)}
			value={AdvancedSettings.expressionCloneShortcutsEnabled}
			onChange={AdvancedSettings.setExpressionCloneShortcutsEnabled}
			compact
			data-flx="user.advanced-settings-tab.switch.expression-clone-shortcuts"
		/>
	);
});
