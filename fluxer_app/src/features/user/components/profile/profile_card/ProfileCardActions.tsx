// SPDX-License-Identifier: AGPL-3.0-or-later

import {ADD_NOTE_DESCRIPTOR, COPY_USER_ID_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import styles from '@app/features/user/components/profile/profile_card/ProfileCardActions.module.css';
import UserNote from '@app/features/user/state/UserNote';
import {EDIT_NOTE_DESCRIPTOR} from '@app/features/user/utils/UserMessageDescriptors';
import {useLingui} from '@lingui/react/macro';
import {ListPlusIcon, SnowflakeIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useRef} from 'react';

interface ProfileCardActionsProps {
	userId: string;
	isHovering: boolean;
	onNoteClick: () => void;
}

export const ProfileCardActions: React.FC<ProfileCardActionsProps> = observer(({userId, isHovering, onNoteClick}) => {
	const {i18n} = useLingui();
	const userNote = UserNote.getUserNote(userId);
	const noteButtonRef = useRef<HTMLButtonElement>(null);
	const copyIdButtonRef = useRef<HTMLButtonElement>(null);
	return (
		<>
			{!StreamerMode.shouldHidePersonalInformation && (
				<div
					className={clsx(styles.noteButtonContainer, isHovering && styles.noteButtonContainerVisible)}
					data-flx="user.profile.profile-card.profile-card-actions.note-button-container"
				>
					<FocusRing
						offset={-2}
						focusTarget={noteButtonRef}
						ringTarget={noteButtonRef}
						data-flx="user.profile.profile-card.profile-card-actions.focus-ring"
					>
						<Tooltip
							text={
								userNote
									? () => (
											<div
												className={styles.noteTooltipContent}
												data-flx="user.profile.profile-card.profile-card-actions.note-tooltip-content"
											>
												{userNote}
											</div>
										)
									: i18n._(ADD_NOTE_DESCRIPTOR)
							}
							maxWidth="none"
							data-flx="user.profile.profile-card.profile-card-actions.tooltip"
						>
							<button
								ref={noteButtonRef}
								type="button"
								onClick={onNoteClick}
								className={styles.noteButton}
								aria-label={userNote ? i18n._(EDIT_NOTE_DESCRIPTOR) : i18n._(ADD_NOTE_DESCRIPTOR)}
								data-flx="user.profile.profile-card.profile-card-actions.note-button.note-click"
							>
								<ListPlusIcon
									className={clsx(styles.iconMedium, styles.noteIconWrapper)}
									data-flx="user.profile.profile-card.profile-card-actions.icon-medium"
								/>
							</button>
						</Tooltip>
					</FocusRing>
				</div>
			)}
			<div
				className={clsx(styles.copyIdButtonContainer, isHovering && styles.copyIdButtonContainerVisible)}
				data-flx="user.profile.profile-card.profile-card-actions.copy-id-button-container"
			>
				<FocusRing
					offset={-2}
					focusTarget={copyIdButtonRef}
					ringTarget={copyIdButtonRef}
					data-flx="user.profile.profile-card.profile-card-actions.focus-ring--2"
				>
					<Tooltip
						text={i18n._(COPY_USER_ID_DESCRIPTOR)}
						maxWidth="none"
						data-flx="user.profile.profile-card.profile-card-actions.tooltip--2"
					>
						<button
							ref={copyIdButtonRef}
							type="button"
							onClick={() => TextCopyCommands.copy(i18n, userId)}
							className={styles.copyIdButton}
							aria-label={i18n._(COPY_USER_ID_DESCRIPTOR)}
							data-flx="user.profile.profile-card.profile-card-actions.copy-id-button"
						>
							<SnowflakeIcon
								weight="bold"
								className={clsx(styles.iconMedium, styles.copyIdIconWrapper)}
								data-flx="user.profile.profile-card.profile-card-actions.icon-medium--2"
							/>
						</button>
					</Tooltip>
				</FocusRing>
			</div>
		</>
	);
});
