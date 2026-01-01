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
import {
	BookmarkIcon,
	CheckIcon,
	DotsThreeOutlineIcon,
	GearIcon,
	HeartIcon,
	LinkSimpleIcon,
	MegaphoneIcon,
	PaperPlaneRightIcon,
	PlayIcon,
	PlusIcon,
	ShareFatIcon,
	TrashIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {
	SettingsTabContainer,
	SettingsTabContent,
	SettingsTabSection,
} from '~/components/modals/shared/SettingsTabLayout';
import {Button} from '~/components/uikit/Button/Button';
import styles from './ButtonsTab.module.css';
import {SubsectionTitle} from './shared';

interface ButtonsTabProps {
	openContextMenu: (event: React.MouseEvent<HTMLElement>) => void;
}

export const ButtonsTab: React.FC<ButtonsTabProps> = observer(({openContextMenu}) => {
	const {t} = useLingui();
	return (
		<SettingsTabContainer>
			<SettingsTabContent>
				<SettingsTabSection
					title={<Trans>Button Variants</Trans>}
					description={<Trans>Click any button to see toast notifications with feedback.</Trans>}
				>
					<div className={styles.buttonsWrapper}>
						<Button
							leftIcon={<PlusIcon size={16} weight="bold" />}
							onClick={() => ToastActionCreators.createToast({type: 'success', children: t`Primary button clicked!`})}
						>
							<Trans>Primary</Trans>
						</Button>
						<Button
							variant="secondary"
							onClick={() => ToastActionCreators.createToast({type: 'success', children: t`Secondary button clicked!`})}
						>
							<Trans>Secondary</Trans>
						</Button>
						<Button
							variant="danger-primary"
							onClick={() => ToastActionCreators.createToast({type: 'error', children: t`Danger primary clicked!`})}
						>
							<Trans>Danger Primary</Trans>
						</Button>
						<Button
							variant="danger-secondary"
							onClick={() => ToastActionCreators.createToast({type: 'error', children: t`Danger secondary clicked!`})}
						>
							<Trans>Danger Secondary</Trans>
						</Button>
						<Button
							variant="inverted"
							onClick={() => ToastActionCreators.createToast({type: 'success', children: t`Inverted button clicked!`})}
						>
							<Trans>Inverted</Trans>
						</Button>
					</div>
				</SettingsTabSection>

				<SettingsTabSection title={<Trans>Disabled States</Trans>}>
					<div className={styles.buttonsWrapper}>
						<Button disabled>
							<Trans>Primary (Disabled)</Trans>
						</Button>
						<Button variant="secondary" disabled>
							<Trans>Secondary (Disabled)</Trans>
						</Button>
						<Button variant="danger-primary" disabled>
							<Trans>Danger (Disabled)</Trans>
						</Button>
					</div>
				</SettingsTabSection>

				<SettingsTabSection title={<Trans>Button Sizes</Trans>}>
					<div className={styles.buttonsWrapper}>
						<Button
							small
							leftIcon={<MegaphoneIcon size={14} />}
							onClick={() => ToastActionCreators.createToast({type: 'success', children: t`Small button clicked!`})}
						>
							<Trans>Small Button</Trans>
						</Button>
						<Button
							leftIcon={<MegaphoneIcon size={16} />}
							onClick={() => ToastActionCreators.createToast({type: 'success', children: t`Regular button clicked!`})}
						>
							<Trans>Regular Button</Trans>
						</Button>
						<Button
							small
							variant="secondary"
							leftIcon={<GearIcon size={14} />}
							onClick={() => ToastActionCreators.createToast({type: 'success', children: t`Small secondary clicked!`})}
						>
							<Trans>Small Secondary</Trans>
						</Button>
					</div>
				</SettingsTabSection>

				<SettingsTabSection title={<Trans>Buttons with Icons</Trans>}>
					<SubsectionTitle>
						<Trans>Left Icon</Trans>
					</SubsectionTitle>
					<div className={styles.buttonsWrapper}>
						<Button
							leftIcon={<PlusIcon size={16} weight="bold" />}
							onClick={() => ToastActionCreators.createToast({type: 'success', children: t`Add action!`})}
						>
							<Trans>Add Item</Trans>
						</Button>
						<Button
							variant="secondary"
							leftIcon={<GearIcon size={16} />}
							onClick={() => ToastActionCreators.createToast({type: 'success', children: t`Settings opened!`})}
						>
							<Trans>Settings</Trans>
						</Button>
						<Button
							variant="danger-primary"
							leftIcon={<TrashIcon size={16} />}
							onClick={() => ToastActionCreators.createToast({type: 'error', children: t`Delete action!`})}
						>
							<Trans>Delete</Trans>
						</Button>
						<Button
							leftIcon={<ShareFatIcon size={16} />}
							onClick={() => ToastActionCreators.createToast({type: 'success', children: t`Share action!`})}
						>
							<Trans>Share</Trans>
						</Button>
					</div>

					<SubsectionTitle>
						<Trans>Right Icon</Trans>
					</SubsectionTitle>
					<div className={styles.buttonsWrapper}>
						<Button
							rightIcon={<PaperPlaneRightIcon size={16} />}
							onClick={() => ToastActionCreators.createToast({type: 'success', children: t`Message sent!`})}
						>
							<Trans>Send</Trans>
						</Button>
						<Button
							variant="secondary"
							rightIcon={<LinkSimpleIcon size={16} weight="bold" />}
							onClick={() => ToastActionCreators.createToast({type: 'success', children: t`Link copied!`})}
						>
							<Trans>Copy Link</Trans>
						</Button>
					</div>

					<SubsectionTitle>
						<Trans>Both Sides</Trans>
					</SubsectionTitle>
					<div className={styles.buttonsWrapper}>
						<Button
							leftIcon={<PlusIcon size={16} weight="bold" />}
							rightIcon={<ShareFatIcon size={16} />}
							onClick={() => ToastActionCreators.createToast({type: 'success', children: t`Action with both icons!`})}
						>
							<Trans>Create & Share</Trans>
						</Button>
						<Button
							variant="secondary"
							leftIcon={<HeartIcon size={16} />}
							rightIcon={<CheckIcon size={16} weight="bold" />}
							onClick={() => ToastActionCreators.createToast({type: 'success', children: t`Saved!`})}
						>
							<Trans>Save Favorite</Trans>
						</Button>
					</div>
				</SettingsTabSection>

				<SettingsTabSection
					title={<Trans>Square Icon Buttons</Trans>}
					description={<Trans>Compact buttons with just an icon, perfect for toolbars and action bars.</Trans>}
				>
					<div className={styles.buttonsWrapper}>
						<Button
							square
							aria-label={t`Play`}
							icon={<PlayIcon size={16} />}
							onClick={() => ToastActionCreators.createToast({type: 'success', children: t`Play!`})}
						/>
						<Button
							square
							aria-label={t`Settings`}
							icon={<GearIcon size={16} />}
							onClick={() => ToastActionCreators.createToast({type: 'success', children: t`Settings!`})}
						/>
						<Button
							square
							variant="secondary"
							aria-label={t`Bookmark`}
							icon={<BookmarkIcon size={16} />}
							onClick={() => ToastActionCreators.createToast({type: 'success', children: t`Bookmarked!`})}
						/>
						<Button
							square
							variant="secondary"
							aria-label={t`Heart`}
							icon={<HeartIcon size={16} />}
							onClick={() => ToastActionCreators.createToast({type: 'success', children: t`Liked!`})}
						/>
						<Button
							square
							variant="danger-primary"
							aria-label={t`Delete`}
							icon={<TrashIcon size={16} />}
							onClick={() => ToastActionCreators.createToast({type: 'error', children: t`Deleted!`})}
						/>
						<Button square aria-label={t`More`} icon={<DotsThreeOutlineIcon size={16} />} onClick={openContextMenu} />
					</div>
				</SettingsTabSection>

				<SettingsTabSection
					title={<Trans>Loading States</Trans>}
					description={<Trans>Buttons show a loading indicator when submitting is true.</Trans>}
				>
					<div className={styles.buttonsWrapper}>
						<Button submitting>
							<Trans>Submitting</Trans>
						</Button>
						<Button variant="secondary" submitting>
							<Trans>Loading</Trans>
						</Button>
						<Button small submitting leftIcon={<MegaphoneIcon size={14} />}>
							<Trans>Small Submitting</Trans>
						</Button>
						<Button variant="danger-primary" submitting>
							<Trans>Processing</Trans>
						</Button>
					</div>
				</SettingsTabSection>

				<SettingsTabSection
					title={<Trans>Button with Context Menu</Trans>}
					description={
						<Trans>
							Buttons can trigger context menus on click by passing the onClick event directly to openContextMenu.
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
					</div>
				</SettingsTabSection>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});
