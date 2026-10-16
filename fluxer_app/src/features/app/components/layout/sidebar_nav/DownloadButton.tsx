// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildListActionButton} from '@app/features/app/components/layout/sidebar_nav/GuildListActionButton';
import styles from '@app/features/app/components/layout/sidebar_nav/GuildListActionButton.module.css';
import {DESKTOP_DOWNLOAD_URL} from '@app/features/app/config/I18nDisplayConstants';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import HiddenGuildListButtons from '@app/features/guild/state/HiddenGuildListButtons';
import {openExternalUrlWithWarning} from '@app/features/messaging/utils/ExternalLinkUtils';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {DownloadSimpleIcon, EyeSlashIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const DOWNLOAD_DESCRIPTOR = msg({
	message: 'Download {productName}',
	comment: 'Short label in the sidebar navigation download button. Preserve {productName}; it is inserted by code.',
});
export const DownloadButton = observer(() => {
	const {i18n} = useLingui();
	if (HiddenGuildListButtons.downloadButtonHidden) {
		return null;
	}
	const handleDownload = () => {
		openExternalUrlWithWarning(DESKTOP_DOWNLOAD_URL);
	};
	const handleContextMenu = (e: React.MouseEvent) => {
		e.preventDefault();
		e.stopPropagation();
		ContextMenuCommands.openFromEvent(e, ({onClose}) => (
			<MenuGroup data-flx="app.sidebar-nav.download-button.handle-context-menu.menu-group">
				<MenuItem
					icon={
						<EyeSlashIcon
							className={styles.menuIcon}
							data-flx="app.sidebar-nav.download-button.handle-context-menu.menu-icon"
						/>
					}
					onClick={() => {
						HiddenGuildListButtons.hideDownloadButton();
						onClose();
					}}
					data-flx="app.sidebar-nav.download-button.handle-context-menu.menu-item.hide-download-button"
				>
					<Trans>Hide download button</Trans>
				</MenuItem>
			</MenuGroup>
		));
	};
	return (
		<GuildListActionButton
			label={i18n._(DOWNLOAD_DESCRIPTOR, {productName: RuntimeConfig.productName})}
			tooltip={() => i18n._(DOWNLOAD_DESCRIPTOR, {productName: RuntimeConfig.productName})}
			icon={
				<DownloadSimpleIcon
					weight="bold"
					className={styles.iconText}
					data-flx="app.sidebar-nav.download-button.icon-text"
				/>
			}
			onClick={handleDownload}
			onContextMenu={handleContextMenu}
			buttonDataFlx="app.sidebar-nav.download-button.button.download"
			data-flx="app.sidebar-nav.download-button"
		/>
	);
});
