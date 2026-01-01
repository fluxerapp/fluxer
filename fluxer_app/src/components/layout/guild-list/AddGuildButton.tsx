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
import {HouseIcon, LinkIcon, PlusIcon} from '@phosphor-icons/react';
import {motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {AddGuildModal, type AddGuildModalView} from '~/components/modals/AddGuildModal';
import {MenuGroup} from '~/components/uikit/ContextMenu/MenuGroup';
import {MenuItem} from '~/components/uikit/ContextMenu/MenuItem';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {TooltipWithKeybind} from '~/components/uikit/KeybindHint/KeybindHint';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useHover} from '~/hooks/useHover';
import {useMergeRefs} from '~/hooks/useMergeRefs';
import guildStyles from '../GuildsLayout.module.css';
import styles from './AddGuildButton.module.css';

export const AddGuildButton = observer(() => {
	const {t} = useLingui();
	const [hoverRef, isHovering] = useHover();
	const buttonRef = React.useRef<HTMLButtonElement | null>(null);
	const iconRef = React.useRef<HTMLDivElement | null>(null);
	const mergedButtonRef = useMergeRefs([hoverRef, buttonRef]);

	const handleAddGuild = (view?: AddGuildModalView) => {
		ModalActionCreators.push(modal(() => <AddGuildModal initialView={view} />));
	};

	const handleContextMenu = (e: React.MouseEvent) => {
		e.preventDefault();
		e.stopPropagation();
		ContextMenuActionCreators.openFromEvent(e, ({onClose}) => (
			<MenuGroup>
				<MenuItem
					icon={<HouseIcon className={styles.menuIcon} />}
					onClick={() => {
						handleAddGuild('create_guild');
						onClose();
					}}
				>
					<Trans>Create Community</Trans>
				</MenuItem>
				<MenuItem
					icon={<LinkIcon className={styles.menuIcon} weight="regular" />}
					onClick={() => {
						handleAddGuild('join_guild');
						onClose();
					}}
				>
					<Trans>Join Community</Trans>
				</MenuItem>
			</MenuGroup>
		));
	};

	return (
		<div className={guildStyles.addGuildButton}>
			<Tooltip
				position="right"
				size="large"
				text={() => <TooltipWithKeybind label={t`Add a Community`} action="create_or_join_server" />}
			>
				<FocusRing offset={-2} focusTarget={buttonRef} ringTarget={iconRef}>
					<button
						type="button"
						aria-label={t`Add a Community`}
						onClick={() => handleAddGuild()}
						onContextMenu={handleContextMenu}
						className={styles.button}
						ref={mergedButtonRef}
					>
						<motion.div
							ref={iconRef}
							className={guildStyles.addGuildButtonIcon}
							animate={{borderRadius: isHovering ? '30%' : '50%'}}
							initial={{borderRadius: isHovering ? '30%' : '50%'}}
							transition={{duration: 0.07, ease: 'easeOut'}}
							whileHover={{borderRadius: '30%'}}
						>
							<PlusIcon weight="bold" className={styles.iconText} />
						</motion.div>
					</button>
				</FocusRing>
			</Tooltip>
		</div>
	);
});
