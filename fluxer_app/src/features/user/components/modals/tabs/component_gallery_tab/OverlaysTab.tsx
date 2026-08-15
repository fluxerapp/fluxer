// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	SettingsTabContainer,
	SettingsTabContent,
	SettingsTabSection,
} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import styles from '@app/features/user/components/modals/tabs/component_gallery_tab/OverlaysTab.module.css';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {DotsThreeOutlineIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const I_AM_A_TOOLTIP_DESCRIPTOR = msg({
	message: 'I am a tooltip',
	comment: 'Tooltip text in the overlays tab. Keep it concise.',
});
const TOP_TOOLTIP_DESCRIPTOR = msg({
	message: 'Top tooltip',
	comment: 'Tooltip text in the overlays tab. Keep it concise.',
});
const RIGHT_TOOLTIP_DESCRIPTOR = msg({
	message: 'Right tooltip',
	comment: 'Tooltip text in the overlays tab. Keep it concise.',
});
const BOTTOM_TOOLTIP_DESCRIPTOR = msg({
	message: 'Bottom tooltip',
	comment: 'Tooltip text in the overlays tab. Keep it concise.',
});
const LEFT_TOOLTIP_DESCRIPTOR = msg({
	message: 'Left tooltip',
	comment: 'Tooltip text in the overlays tab. Keep it concise.',
});
const GREAT_SUCCESS_DESCRIPTOR = msg({
	message: 'Great success.',
	comment: 'Short label in the overlays tab. Keep it concise.',
});
const SOMETHING_WENT_WRONG_DESCRIPTOR = msg({
	message: 'Something went wrong.',
	comment: 'Short label in the overlays tab. Keep it concise.',
});
const OPEN_MENU_ICON_DESCRIPTOR = msg({
	message: 'Open menu icon',
	comment: 'Button or menu action label in the overlays tab. Keep it concise.',
});

interface OverlaysTabProps {
	openContextMenu: (event: React.MouseEvent<HTMLElement>) => void;
}

export const OverlaysTab: React.FC<OverlaysTabProps> = observer(({openContextMenu}) => {
	const {i18n} = useLingui();
	return (
		<SettingsTabContainer data-flx="user.component-gallery-tab.overlays-tab.settings-tab-container">
			<SettingsTabContent data-flx="user.component-gallery-tab.overlays-tab.settings-tab-content">
				<SettingsTabSection
					title={<Trans>Tooltips</Trans>}
					description={<Trans>Hover over buttons to see tooltips in different positions.</Trans>}
					data-flx="user.component-gallery-tab.overlays-tab.settings-tab-section"
				>
					<div className={styles.buttonsWrapper} data-flx="user.component-gallery-tab.overlays-tab.buttons-wrapper">
						<Tooltip
							text={i18n._(I_AM_A_TOOLTIP_DESCRIPTOR)}
							data-flx="user.component-gallery-tab.overlays-tab.tooltip"
						>
							<Button data-flx="user.component-gallery-tab.overlays-tab.button">
								<Trans>Hover me</Trans>
							</Button>
						</Tooltip>
						<Tooltip
							text={i18n._(TOP_TOOLTIP_DESCRIPTOR)}
							position="top"
							data-flx="user.component-gallery-tab.overlays-tab.tooltip--2"
						>
							<Button variant="secondary" data-flx="user.component-gallery-tab.overlays-tab.button--2">
								<Trans>Top</Trans>
							</Button>
						</Tooltip>
						<Tooltip
							text={i18n._(RIGHT_TOOLTIP_DESCRIPTOR)}
							position="right"
							data-flx="user.component-gallery-tab.overlays-tab.tooltip--3"
						>
							<Button variant="secondary" data-flx="user.component-gallery-tab.overlays-tab.button--3">
								<Trans>Right</Trans>
							</Button>
						</Tooltip>
						<Tooltip
							text={i18n._(BOTTOM_TOOLTIP_DESCRIPTOR)}
							position="bottom"
							data-flx="user.component-gallery-tab.overlays-tab.tooltip--4"
						>
							<Button variant="secondary" data-flx="user.component-gallery-tab.overlays-tab.button--4">
								<Trans>Bottom</Trans>
							</Button>
						</Tooltip>
						<Tooltip
							text={i18n._(LEFT_TOOLTIP_DESCRIPTOR)}
							position="left"
							data-flx="user.component-gallery-tab.overlays-tab.tooltip--5"
						>
							<Button variant="secondary" data-flx="user.component-gallery-tab.overlays-tab.button--5">
								<Trans>Left</Trans>
							</Button>
						</Tooltip>
					</div>
				</SettingsTabSection>
				<SettingsTabSection
					title={<Trans>Toasts</Trans>}
					description={<Trans>Toasts appear in the top-center of the screen.</Trans>}
					data-flx="user.component-gallery-tab.overlays-tab.settings-tab-section--2"
				>
					<div className={styles.buttonsWrapper} data-flx="user.component-gallery-tab.overlays-tab.buttons-wrapper--2">
						<Button
							onClick={() => ToastCommands.createToast({type: 'success', children: i18n._(GREAT_SUCCESS_DESCRIPTOR)})}
							data-flx="user.component-gallery-tab.overlays-tab.button.create-toast"
						>
							<Trans>Success</Trans>
						</Button>
						<Button
							variant="danger"
							onClick={() =>
								ToastCommands.createToast({type: 'error', children: i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR)})
							}
							data-flx="user.component-gallery-tab.overlays-tab.button.create-toast--2"
						>
							<Trans>Error</Trans>
						</Button>
					</div>
				</SettingsTabSection>
				<SettingsTabSection
					title={<Trans>Context menus</Trans>}
					description={
						<Trans>
							Context menus can be opened with left-click (on buttons) or right-click (on other elements). This
							demonstrates various menu items including checkboxes, radio buttons, sliders, and submenus.
						</Trans>
					}
					data-flx="user.component-gallery-tab.overlays-tab.settings-tab-section--3"
				>
					<div className={styles.buttonsWrapper} data-flx="user.component-gallery-tab.overlays-tab.buttons-wrapper--3">
						<Button
							leftIcon={
								<DotsThreeOutlineIcon
									size={remFromPx(16)}
									data-flx="user.component-gallery-tab.overlays-tab.dots-three-outline-icon"
								/>
							}
							onClick={openContextMenu}
							data-flx="user.component-gallery-tab.overlays-tab.button.open-context-menu"
						>
							<Trans>Open menu</Trans>
						</Button>
						<Button
							square
							icon={
								<DotsThreeOutlineIcon
									size={remFromPx(16)}
									data-flx="user.component-gallery-tab.overlays-tab.dots-three-outline-icon--2"
								/>
							}
							aria-label={i18n._(OPEN_MENU_ICON_DESCRIPTOR)}
							onClick={openContextMenu}
							data-flx="user.component-gallery-tab.overlays-tab.button.open-context-menu--2"
						/>
						{/* biome-ignore lint/a11y/noStaticElementInteractions: gallery demo target for the context-menu sample. */}
						<div
							onContextMenu={openContextMenu}
							className={styles.demoArea}
							data-flx="user.component-gallery-tab.overlays-tab.demo-area.open-context-menu"
						>
							<Trans>Right-click here to open the context menu</Trans>
						</div>
					</div>
				</SettingsTabSection>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});
