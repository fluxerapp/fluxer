// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AccessibilityCommands from '@app/features/accessibility/commands/AccessibilityCommands';
import {HelpCenterArticleSlug} from '@app/features/app/config/HelpCenterConstants';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as HelpCenterUtils from '@app/features/ui/utils/HelpCenterUtils';
import {openExternalUrl} from '@app/features/ui/utils/NativeUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {type FC, useCallback} from 'react';

const HIDE_EXPIRY_FOOTNOTES_DESCRIPTOR = msg({
	message: 'Hide expiration footnotes',
	comment: 'Short label in the shared app expiry footnote context menu.',
});
const VIEW_HELP_ARTICLE_DESCRIPTOR = msg({
	message: 'View help article',
	comment: 'Short label in the shared app expiry footnote context menu.',
});
export const ExpiryFootnoteContextMenu: FC = () => {
	const {i18n} = useLingui();
	const helpUrl = HelpCenterUtils.getURL(HelpCenterArticleSlug.AttachmentExpiry);
	const handleHideFootnotes = useCallback(() => {
		AccessibilityCommands.update({showAttachmentExpiryIndicator: false});
	}, []);
	const handleOpenHelpCenter = useCallback(() => {
		void openExternalUrl(helpUrl);
	}, [helpUrl]);
	return (
		<MenuGroup data-flx="app.expiry-footnote-context-menu.menu-group">
			<MenuItem onClick={handleHideFootnotes} data-flx="app.expiry-footnote-context-menu.menu-item.hide-footnotes">
				{i18n._(HIDE_EXPIRY_FOOTNOTES_DESCRIPTOR)}
			</MenuItem>
			<MenuItem onClick={handleOpenHelpCenter} data-flx="app.expiry-footnote-context-menu.menu-item.open-help-center">
				{i18n._(VIEW_HELP_ARTICLE_DESCRIPTOR)}
			</MenuItem>
		</MenuGroup>
	);
};
