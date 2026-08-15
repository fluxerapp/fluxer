// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	SettingsTabContainer,
	SettingsTabContent,
	SettingsTabSection,
} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {PLAY_DESCRIPTOR, SETTINGS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import styles from '@app/features/user/components/modals/tabs/component_gallery_tab/ComponentGalleryTabButtonsTab.module.css';
import {SubsectionTitle} from '@app/features/user/components/modals/tabs/component_gallery_tab/ComponentGalleryTabSubsectionTitle';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {
	BookmarkSimpleIcon,
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

const PRIMARY_BUTTON_CLICKED_DESCRIPTOR = msg({
	message: 'Primary button clicked.',
	comment: 'Short label in the buttons tab. Keep it concise.',
});
const SECONDARY_BUTTON_CLICKED_DESCRIPTOR = msg({
	message: 'Secondary button clicked.',
	comment: 'Short label in the buttons tab. Keep it concise.',
});
const DANGER_BUTTON_CLICKED_DESCRIPTOR = msg({
	message: 'Danger button clicked.',
	comment: 'Short label in the buttons tab. Keep it concise. Keep the tone plain and specific.',
});
const INVERTED_BUTTON_CLICKED_DESCRIPTOR = msg({
	message: 'Inverted button clicked.',
	comment: 'Short label in the buttons tab. Keep it concise.',
});
const SMALL_BUTTON_CLICKED_DESCRIPTOR = msg({
	message: 'Small button clicked.',
	comment: 'Short label in the buttons tab. Keep it concise.',
});
const REGULAR_BUTTON_CLICKED_DESCRIPTOR = msg({
	message: 'Regular button clicked.',
	comment: 'Short label in the buttons tab. Keep it concise.',
});
const SMALL_SECONDARY_CLICKED_DESCRIPTOR = msg({
	message: 'Small secondary clicked.',
	comment: 'Short label in the buttons tab. Keep it concise.',
});
const ADD_ACTION_DESCRIPTOR = msg({
	message: 'Add action.',
	comment: 'Button or menu action label in the buttons tab. Keep it concise.',
});
const SETTINGS_OPENED_DESCRIPTOR = msg({
	message: 'Settings opened.',
	comment: 'Short label in the buttons tab. Keep it concise.',
});
const DELETE_ACTION_DESCRIPTOR = msg({
	message: 'Delete action.',
	comment: 'Button or menu action label in the buttons tab. Keep it concise. Keep the tone plain and specific.',
});
const SHARE_ACTION_DESCRIPTOR = msg({
	message: 'Share action.',
	comment: 'Button or menu action label in the buttons tab. Keep it concise.',
});
const MESSAGE_SENT_DESCRIPTOR = msg({
	message: 'Message sent.',
	comment: 'Short label in the buttons tab. Keep it concise.',
});
const LINK_COPIED_DESCRIPTOR = msg({
	message: 'Link copied.',
	comment: 'Short label in the buttons tab. Keep it concise.',
});
const ACTION_WITH_BOTH_ICONS_DESCRIPTOR = msg({
	message: 'Action with both icons.',
	comment: 'Label in the buttons tab.',
});
const SAVED_DESCRIPTOR = msg({
	message: 'Saved.',
	comment: 'Button or menu action label in the buttons tab. Keep it concise.',
});
const PLAY_2_DESCRIPTOR = msg({
	message: 'Play.',
	comment: 'Short label in the buttons tab. Keep it concise.',
});
const SETTINGS_2_DESCRIPTOR = msg({
	message: 'Settings.',
	comment: 'Short label in the buttons tab. Keep it concise.',
});
const BOOKMARK_DESCRIPTOR = msg({
	message: 'Bookmark',
	comment: 'Short label in the buttons tab. Keep it concise.',
});
const BOOKMARKED_DESCRIPTOR = msg({
	message: 'Bookmarked.',
	comment: 'Short label in the buttons tab. Keep it concise.',
});
const HEART_DESCRIPTOR = msg({
	message: 'Heart',
	comment: 'Short label in the buttons tab. Keep it concise.',
});
const LIKED_DESCRIPTOR = msg({
	message: 'Liked.',
	comment: 'Short label in the buttons tab. Keep it concise.',
});
const DELETE_DESCRIPTOR = msg({
	message: 'Delete',
	comment: 'Button or menu action label in the buttons tab. Keep it concise. Keep the tone plain and specific.',
});
const DELETED_DESCRIPTOR = msg({
	message: 'Deleted.',
	comment: 'Button or menu action label in the buttons tab. Keep it concise. Keep the tone plain and specific.',
});
const MORE_DESCRIPTOR = msg({
	message: 'More',
	comment: 'Short label in the buttons tab. Keep it concise.',
});
const OPEN_MENU_ICON_DESCRIPTOR = msg({
	message: 'Open menu icon',
	comment: 'Button or menu action label in the buttons tab. Keep it concise.',
});

interface ButtonsTabProps {
	openContextMenu: (event: React.MouseEvent<HTMLElement>) => void;
}

export const ButtonsTab: React.FC<ButtonsTabProps> = observer(({openContextMenu}) => {
	const {i18n} = useLingui();
	return (
		<SettingsTabContainer data-flx="user.component-gallery-tab.buttons-tab.settings-tab-container">
			<SettingsTabContent data-flx="user.component-gallery-tab.buttons-tab.settings-tab-content">
				<SettingsTabSection
					title={<Trans>Button variants</Trans>}
					description={<Trans>Click any button to see toast notifications with feedback.</Trans>}
					data-flx="user.component-gallery-tab.buttons-tab.settings-tab-section"
				>
					<div className={styles.buttonsWrapper} data-flx="user.component-gallery-tab.buttons-tab.buttons-wrapper">
						<Button
							leftIcon={
								<PlusIcon
									size={remFromPx(16)}
									weight="bold"
									data-flx="user.component-gallery-tab.buttons-tab.plus-icon"
								/>
							}
							onClick={() =>
								ToastCommands.createToast({type: 'success', children: i18n._(PRIMARY_BUTTON_CLICKED_DESCRIPTOR)})
							}
							data-flx="user.component-gallery-tab.buttons-tab.button.create-toast"
						>
							<Trans>Primary</Trans>
						</Button>
						<Button
							variant="secondary"
							onClick={() =>
								ToastCommands.createToast({type: 'success', children: i18n._(SECONDARY_BUTTON_CLICKED_DESCRIPTOR)})
							}
							data-flx="user.component-gallery-tab.buttons-tab.button.create-toast--2"
						>
							<Trans>Secondary</Trans>
						</Button>
						<Button
							variant="danger"
							onClick={() =>
								ToastCommands.createToast({type: 'error', children: i18n._(DANGER_BUTTON_CLICKED_DESCRIPTOR)})
							}
							data-flx="user.component-gallery-tab.buttons-tab.button.create-toast--3"
						>
							<Trans>Danger</Trans>
						</Button>
						<Button
							variant="inverted"
							onClick={() =>
								ToastCommands.createToast({type: 'success', children: i18n._(INVERTED_BUTTON_CLICKED_DESCRIPTOR)})
							}
							data-flx="user.component-gallery-tab.buttons-tab.button.create-toast--5"
						>
							<Trans>Inverted</Trans>
						</Button>
					</div>
				</SettingsTabSection>
				<SettingsTabSection
					title={<Trans>Disabled states</Trans>}
					data-flx="user.component-gallery-tab.buttons-tab.settings-tab-section--2"
				>
					<div className={styles.buttonsWrapper} data-flx="user.component-gallery-tab.buttons-tab.buttons-wrapper--2">
						<Button disabled data-flx="user.component-gallery-tab.buttons-tab.button">
							<Trans>Primary (disabled)</Trans>
						</Button>
						<Button variant="secondary" disabled data-flx="user.component-gallery-tab.buttons-tab.button--2">
							<Trans>Secondary (disabled)</Trans>
						</Button>
						<Button variant="danger" disabled data-flx="user.component-gallery-tab.buttons-tab.button--3">
							<Trans>Danger (disabled)</Trans>
						</Button>
					</div>
				</SettingsTabSection>
				<SettingsTabSection
					title={<Trans>Button sizes</Trans>}
					data-flx="user.component-gallery-tab.buttons-tab.settings-tab-section--3"
				>
					<div className={styles.buttonsWrapper} data-flx="user.component-gallery-tab.buttons-tab.buttons-wrapper--3">
						<Button
							small
							leftIcon={
								<MegaphoneIcon size={remFromPx(14)} data-flx="user.component-gallery-tab.buttons-tab.megaphone-icon" />
							}
							onClick={() =>
								ToastCommands.createToast({type: 'success', children: i18n._(SMALL_BUTTON_CLICKED_DESCRIPTOR)})
							}
							data-flx="user.component-gallery-tab.buttons-tab.button.create-toast--6"
						>
							<Trans>Small button</Trans>
						</Button>
						<Button
							leftIcon={
								<MegaphoneIcon
									size={remFromPx(16)}
									data-flx="user.component-gallery-tab.buttons-tab.megaphone-icon--2"
								/>
							}
							onClick={() =>
								ToastCommands.createToast({type: 'success', children: i18n._(REGULAR_BUTTON_CLICKED_DESCRIPTOR)})
							}
							data-flx="user.component-gallery-tab.buttons-tab.button.create-toast--7"
						>
							<Trans>Regular button</Trans>
						</Button>
						<Button
							small
							variant="secondary"
							leftIcon={<GearIcon size={remFromPx(14)} data-flx="user.component-gallery-tab.buttons-tab.gear-icon" />}
							onClick={() =>
								ToastCommands.createToast({type: 'success', children: i18n._(SMALL_SECONDARY_CLICKED_DESCRIPTOR)})
							}
							data-flx="user.component-gallery-tab.buttons-tab.button.create-toast--8"
						>
							<Trans>Small secondary</Trans>
						</Button>
					</div>
				</SettingsTabSection>
				<SettingsTabSection
					title={<Trans>Buttons with icons</Trans>}
					data-flx="user.component-gallery-tab.buttons-tab.settings-tab-section--4"
				>
					<SubsectionTitle data-flx="user.component-gallery-tab.buttons-tab.subsection-title">
						<Trans>Left icon</Trans>
					</SubsectionTitle>
					<div className={styles.buttonsWrapper} data-flx="user.component-gallery-tab.buttons-tab.buttons-wrapper--4">
						<Button
							leftIcon={
								<PlusIcon
									size={remFromPx(16)}
									weight="bold"
									data-flx="user.component-gallery-tab.buttons-tab.plus-icon--2"
								/>
							}
							onClick={() => ToastCommands.createToast({type: 'success', children: i18n._(ADD_ACTION_DESCRIPTOR)})}
							data-flx="user.component-gallery-tab.buttons-tab.button.create-toast--9"
						>
							<Trans>Add item</Trans>
						</Button>
						<Button
							variant="secondary"
							leftIcon={
								<GearIcon size={remFromPx(16)} data-flx="user.component-gallery-tab.buttons-tab.gear-icon--2" />
							}
							onClick={() => ToastCommands.createToast({type: 'success', children: i18n._(SETTINGS_OPENED_DESCRIPTOR)})}
							data-flx="user.component-gallery-tab.buttons-tab.button.create-toast--10"
						>
							<Trans>Settings</Trans>
						</Button>
						<Button
							variant="danger"
							leftIcon={<TrashIcon size={remFromPx(16)} data-flx="user.component-gallery-tab.buttons-tab.trash-icon" />}
							onClick={() => ToastCommands.createToast({type: 'error', children: i18n._(DELETE_ACTION_DESCRIPTOR)})}
							data-flx="user.component-gallery-tab.buttons-tab.button.create-toast--11"
						>
							<Trans>Delete</Trans>
						</Button>
						<Button
							leftIcon={
								<ShareFatIcon size={remFromPx(16)} data-flx="user.component-gallery-tab.buttons-tab.share-fat-icon" />
							}
							onClick={() => ToastCommands.createToast({type: 'success', children: i18n._(SHARE_ACTION_DESCRIPTOR)})}
							data-flx="user.component-gallery-tab.buttons-tab.button.create-toast--12"
						>
							<Trans>Share</Trans>
						</Button>
					</div>
					<SubsectionTitle data-flx="user.component-gallery-tab.buttons-tab.subsection-title--2">
						<Trans>Right icon</Trans>
					</SubsectionTitle>
					<div className={styles.buttonsWrapper} data-flx="user.component-gallery-tab.buttons-tab.buttons-wrapper--5">
						<Button
							rightIcon={
								<PaperPlaneRightIcon
									size={remFromPx(16)}
									data-flx="user.component-gallery-tab.buttons-tab.paper-plane-right-icon"
								/>
							}
							onClick={() => ToastCommands.createToast({type: 'success', children: i18n._(MESSAGE_SENT_DESCRIPTOR)})}
							data-flx="user.component-gallery-tab.buttons-tab.button.create-toast--13"
						>
							<Trans>Send</Trans>
						</Button>
						<Button
							variant="secondary"
							rightIcon={
								<LinkSimpleIcon
									size={remFromPx(16)}
									weight="bold"
									data-flx="user.component-gallery-tab.buttons-tab.link-simple-icon"
								/>
							}
							onClick={() => ToastCommands.createToast({type: 'success', children: i18n._(LINK_COPIED_DESCRIPTOR)})}
							data-flx="user.component-gallery-tab.buttons-tab.button.create-toast--14"
						>
							<Trans>Copy link</Trans>
						</Button>
					</div>
					<SubsectionTitle data-flx="user.component-gallery-tab.buttons-tab.subsection-title--3">
						<Trans>Both sides</Trans>
					</SubsectionTitle>
					<div className={styles.buttonsWrapper} data-flx="user.component-gallery-tab.buttons-tab.buttons-wrapper--6">
						<Button
							leftIcon={
								<PlusIcon
									size={remFromPx(16)}
									weight="bold"
									data-flx="user.component-gallery-tab.buttons-tab.plus-icon--3"
								/>
							}
							rightIcon={
								<ShareFatIcon
									size={remFromPx(16)}
									data-flx="user.component-gallery-tab.buttons-tab.share-fat-icon--2"
								/>
							}
							onClick={() =>
								ToastCommands.createToast({type: 'success', children: i18n._(ACTION_WITH_BOTH_ICONS_DESCRIPTOR)})
							}
							data-flx="user.component-gallery-tab.buttons-tab.button.create-toast--15"
						>
							<Trans>Create & share</Trans>
						</Button>
						<Button
							variant="secondary"
							leftIcon={<HeartIcon size={remFromPx(16)} data-flx="user.component-gallery-tab.buttons-tab.heart-icon" />}
							rightIcon={
								<CheckIcon
									size={remFromPx(16)}
									weight="bold"
									data-flx="user.component-gallery-tab.buttons-tab.check-icon"
								/>
							}
							onClick={() => ToastCommands.createToast({type: 'success', children: i18n._(SAVED_DESCRIPTOR)})}
							data-flx="user.component-gallery-tab.buttons-tab.button.create-toast--16"
						>
							<Trans>Save favorite</Trans>
						</Button>
					</div>
				</SettingsTabSection>
				<SettingsTabSection
					title={<Trans>Square icon buttons</Trans>}
					description={<Trans>Compact buttons with just an icon, perfect for toolbars and action bars.</Trans>}
					data-flx="user.component-gallery-tab.buttons-tab.settings-tab-section--5"
				>
					<div className={styles.buttonsWrapper} data-flx="user.component-gallery-tab.buttons-tab.buttons-wrapper--7">
						<Button
							square
							aria-label={i18n._(PLAY_DESCRIPTOR)}
							icon={<PlayIcon size={remFromPx(16)} data-flx="user.component-gallery-tab.buttons-tab.play-icon" />}
							onClick={() => ToastCommands.createToast({type: 'success', children: i18n._(PLAY_2_DESCRIPTOR)})}
							data-flx="user.component-gallery-tab.buttons-tab.button.create-toast--17"
						/>
						<Button
							square
							aria-label={i18n._(SETTINGS_DESCRIPTOR)}
							icon={<GearIcon size={remFromPx(16)} data-flx="user.component-gallery-tab.buttons-tab.gear-icon--3" />}
							onClick={() => ToastCommands.createToast({type: 'success', children: i18n._(SETTINGS_2_DESCRIPTOR)})}
							data-flx="user.component-gallery-tab.buttons-tab.button.create-toast--18"
						/>
						<Button
							square
							variant="secondary"
							aria-label={i18n._(BOOKMARK_DESCRIPTOR)}
							icon={
								<BookmarkSimpleIcon
									size={remFromPx(16)}
									data-flx="user.component-gallery-tab.buttons-tab.bookmark-simple-icon"
								/>
							}
							onClick={() => ToastCommands.createToast({type: 'success', children: i18n._(BOOKMARKED_DESCRIPTOR)})}
							data-flx="user.component-gallery-tab.buttons-tab.button.create-toast--19"
						/>
						<Button
							square
							variant="secondary"
							aria-label={i18n._(HEART_DESCRIPTOR)}
							icon={<HeartIcon size={remFromPx(16)} data-flx="user.component-gallery-tab.buttons-tab.heart-icon--2" />}
							onClick={() => ToastCommands.createToast({type: 'success', children: i18n._(LIKED_DESCRIPTOR)})}
							data-flx="user.component-gallery-tab.buttons-tab.button.create-toast--20"
						/>
						<Button
							square
							variant="danger"
							aria-label={i18n._(DELETE_DESCRIPTOR)}
							icon={<TrashIcon size={remFromPx(16)} data-flx="user.component-gallery-tab.buttons-tab.trash-icon--2" />}
							onClick={() => ToastCommands.createToast({type: 'error', children: i18n._(DELETED_DESCRIPTOR)})}
							data-flx="user.component-gallery-tab.buttons-tab.button.create-toast--21"
						/>
						<Button
							square
							aria-label={i18n._(MORE_DESCRIPTOR)}
							icon={
								<DotsThreeOutlineIcon
									size={remFromPx(16)}
									data-flx="user.component-gallery-tab.buttons-tab.dots-three-outline-icon"
								/>
							}
							onClick={openContextMenu}
							data-flx="user.component-gallery-tab.buttons-tab.button.open-context-menu"
						/>
					</div>
				</SettingsTabSection>
				<SettingsTabSection
					title={<Trans>Loading states</Trans>}
					description={<Trans>Buttons show a loading indicator when submitting is true.</Trans>}
					data-flx="user.component-gallery-tab.buttons-tab.settings-tab-section--6"
				>
					<div className={styles.buttonsWrapper} data-flx="user.component-gallery-tab.buttons-tab.buttons-wrapper--8">
						<Button submitting data-flx="user.component-gallery-tab.buttons-tab.button--4">
							<Trans>Submitting</Trans>
						</Button>
						<Button variant="secondary" submitting data-flx="user.component-gallery-tab.buttons-tab.button--5">
							<Trans>Loading</Trans>
						</Button>
						<Button
							small
							submitting
							leftIcon={
								<MegaphoneIcon
									size={remFromPx(14)}
									data-flx="user.component-gallery-tab.buttons-tab.megaphone-icon--3"
								/>
							}
							data-flx="user.component-gallery-tab.buttons-tab.button--6"
						>
							<Trans>Small submitting</Trans>
						</Button>
						<Button variant="danger" submitting data-flx="user.component-gallery-tab.buttons-tab.button--7">
							<Trans>Processing</Trans>
						</Button>
					</div>
				</SettingsTabSection>
				<SettingsTabSection
					title={<Trans>Button with context menu</Trans>}
					description={
						<Trans>
							Buttons can trigger context menus on click by passing the onClick event directly to openContextMenu.
						</Trans>
					}
					data-flx="user.component-gallery-tab.buttons-tab.settings-tab-section--7"
				>
					<div className={styles.buttonsWrapper} data-flx="user.component-gallery-tab.buttons-tab.buttons-wrapper--9">
						<Button
							leftIcon={
								<DotsThreeOutlineIcon
									size={remFromPx(16)}
									data-flx="user.component-gallery-tab.buttons-tab.dots-three-outline-icon--2"
								/>
							}
							onClick={openContextMenu}
							data-flx="user.component-gallery-tab.buttons-tab.button.open-context-menu--2"
						>
							<Trans>Open menu</Trans>
						</Button>
						<Button
							square
							icon={
								<DotsThreeOutlineIcon
									size={remFromPx(16)}
									data-flx="user.component-gallery-tab.buttons-tab.dots-three-outline-icon--3"
								/>
							}
							aria-label={i18n._(OPEN_MENU_ICON_DESCRIPTOR)}
							onClick={openContextMenu}
							data-flx="user.component-gallery-tab.buttons-tab.button.open-context-menu--3"
						/>
					</div>
				</SettingsTabSection>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});
