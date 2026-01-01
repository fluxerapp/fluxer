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

import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {WarningIcon} from '@phosphor-icons/react';
import React from 'react';
import styles from './MentionEveryonePopout.module.css';

interface MentionEveryonePopoutProps {
	mentionType: '@everyone' | '@here' | 'role';
	memberCount: number;
	onConfirm: () => void;
	onCancel: () => void;
	roleName?: string;
}

const isMac = () => /Mac|iPod|iPhone|iPad/.test(navigator.platform);

export const getMentionTitle = (
	mentionType: MentionEveryonePopoutProps['mentionType'],
	roleName?: string,
	t?: (msg: import('@lingui/core').MessageDescriptor) => string,
) => {
	if (mentionType === 'role') {
		return <Trans>Mention {roleName ?? t?.(msg`this role`) ?? 'this role'}?</Trans>;
	}
	if (mentionType === '@everyone') {
		return <Trans>Mention @everyone?</Trans>;
	}
	return <Trans>Mention @here?</Trans>;
};

export const getMentionDescription = (
	mentionType: MentionEveryonePopoutProps['mentionType'],
	memberCount: number,
	roleName?: string,
	t?: (msg: import('@lingui/core').MessageDescriptor) => string,
) => {
	if (mentionType === 'role') {
		return (
			<Trans>
				This will notify <strong>{memberCount.toLocaleString()}</strong> members with the{' '}
				<span className={styles.roleName}>{roleName ?? t?.(msg`mentioned role`) ?? 'mentioned role'}</span> in this
				channel. Are you sure you want to do this?
			</Trans>
		);
	}
	if (mentionType === '@everyone') {
		return (
			<Trans>
				This will notify <strong>{memberCount.toLocaleString()}</strong> members in this channel. Are you sure you want
				to do this?
			</Trans>
		);
	}
	return (
		<Trans>
			This will notify up to <strong>{memberCount.toLocaleString()}</strong> online members in this channel. Are you
			sure you want to do this?
		</Trans>
	);
};

export const MentionEveryonePopout = ({
	mentionType,
	memberCount,
	onConfirm,
	onCancel,
	roleName,
}: MentionEveryonePopoutProps) => {
	const {t} = useLingui();
	const handleKeyDown = React.useCallback(
		(event: KeyboardEvent) => {
			if (event.key === 'Escape') {
				event.preventDefault();
				event.stopPropagation();
				onCancel();
				return;
			}

			if (event.key === 'Enter') {
				event.preventDefault();
				event.stopPropagation();
				onConfirm();
			}
		},
		[onCancel, onConfirm],
	);

	React.useEffect(() => {
		document.addEventListener('keydown', handleKeyDown, true);
		return () => document.removeEventListener('keydown', handleKeyDown, true);
	}, [handleKeyDown]);

	const enterKeySymbol = isMac() ? '↵' : 'Enter';

	return (
		<div className={styles.container} role="dialog" aria-modal="true">
			<div className={styles.header}>
				<WarningIcon size={20} weight="fill" className={styles.warningIcon} />
				<span className={styles.title}>{getMentionTitle(mentionType, roleName, t)}</span>
			</div>
			<p className={styles.description}>{getMentionDescription(mentionType, memberCount, roleName, t)}</p>
			<div className={styles.keybinds}>
				<div className={styles.keybind}>
					<kbd className={styles.keybindHint}>Esc</kbd>
					<span>
						<Trans>Cancel</Trans>
					</span>
				</div>
				<div className={styles.keybind}>
					<kbd className={styles.keybindHint}>{enterKeySymbol}</kbd>
					<span>
						<Trans>Confirm</Trans>
					</span>
				</div>
			</div>
		</div>
	);
};
