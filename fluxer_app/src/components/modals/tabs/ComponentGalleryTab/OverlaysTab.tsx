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

import {Trans, useLingui} from '@lingui/react/macro';
import {DotsThreeOutlineIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {
	SettingsTabContainer,
	SettingsTabContent,
	SettingsTabSection,
} from '~/components/modals/shared/SettingsTabLayout';
import {Button} from '~/components/uikit/Button/Button';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import styles from './OverlaysTab.module.css';

interface OverlaysTabProps {
	openContextMenu: (event: React.MouseEvent<HTMLElement>) => void;
}

export const OverlaysTab: React.FC<OverlaysTabProps> = observer(({openContextMenu}) => {
	const {t} = useLingui();
	return (
		<SettingsTabContainer>
			<SettingsTabContent>
				<SettingsTabSection
					title={<Trans>Tooltips</Trans>}
					description={<Trans>Hover over buttons to see tooltips in different positions.</Trans>}
				>
					<div className={styles.buttonsWrapper}>
						<Tooltip text={t`I am a tooltip`}>
							<Button>
								<Trans>Hover Me</Trans>
							</Button>
						</Tooltip>
						<Tooltip text={t`Top Tooltip`} position="top">
							<Button variant="secondary">
								<Trans>Top</Trans>
							</Button>
						</Tooltip>
						<Tooltip text={t`Right Tooltip`} position="right">
							<Button variant="secondary">
								<Trans>Right</Trans>
							</Button>
						</Tooltip>
						<Tooltip text={t`Bottom Tooltip`} position="bottom">
							<Button variant="secondary">
								<Trans>Bottom</Trans>
							</Button>
						</Tooltip>
						<Tooltip text={t`Left Tooltip`} position="left">
							<Button variant="secondary">
								<Trans>Left</Trans>
							</Button>
						</Tooltip>
					</div>
				</SettingsTabSection>

				<SettingsTabSection
					title={<Trans>Toasts</Trans>}
					description={<Trans>Toasts appear in the top-center of the screen.</Trans>}
				>
					<div className={styles.buttonsWrapper}>
						<Button onClick={() => ToastActionCreators.createToast({type: 'success', children: t`Great Success!`})}>
							<Trans>Success</Trans>
						</Button>
						<Button
							variant="danger-primary"
							onClick={() => ToastActionCreators.createToast({type: 'error', children: t`Something went wrong.`})}
						>
							<Trans>Error</Trans>
						</Button>
					</div>
				</SettingsTabSection>

				<SettingsTabSection
					title={<Trans>Context Menus</Trans>}
					description={
						<Trans>
							Context menus can be opened with left-click (on buttons) or right-click (on other elements). This
							demonstrates various menu items including checkboxes, radio buttons, sliders, and submenus.
						</Trans>
					}
				>
					<div className={styles.buttonsWrapper}>
						<Button leftIcon={<DotsThreeOutlineIcon size={16} />} onClick={openContextMenu}>
							<Trans>Open Menu</Trans>
						</Button>
						<Button
							square
							icon={<DotsThreeOutlineIcon size={16} />}
							aria-label={t`Open Menu (icon)`}
							onClick={openContextMenu}
						/>
						<div role="button" tabIndex={0} onContextMenu={openContextMenu} className={styles.demoArea}>
							<Trans>Right-click here to open the context menu</Trans>
						</div>
					</div>
				</SettingsTabSection>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});
