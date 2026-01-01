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
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as PackInviteActionCreators from '~/actions/PackInviteActionCreators';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import {Select, type SelectOption} from '~/components/form/Select';
import {Switch} from '~/components/form/Switch';
import * as Modal from '~/components/modals/Modal';
import {CopyLinkSection} from '~/components/modals/shared/CopyLinkSection';
import {Button} from '~/components/uikit/Button/Button';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import styles from './PackInviteModal.module.css';

interface PackInviteModalProps {
	packId: string;
	type: 'emoji' | 'sticker';
	onCreated?: () => void;
}

export const PackInviteModal = observer(({packId, type, onCreated}: PackInviteModalProps) => {
	const {t, i18n} = useLingui();

	const MAX_AGE_OPTIONS: Array<SelectOption<string>> = React.useMemo(
		() => [
			{value: '0', label: t`Never`},
			{value: '1800', label: t`30 minutes`},
			{value: '3600', label: t`1 hour`},
			{value: '21600', label: t`6 hours`},
			{value: '43200', label: t`12 hours`},
			{value: '86400', label: t`1 day`},
			{value: '604800', label: t`7 days`},
		],
		[t],
	);

	const MAX_USES_OPTIONS: Array<SelectOption<string>> = React.useMemo(
		() => [
			{value: '0', label: t`Unlimited`},
			{value: '1', label: t`1 use`},
			{value: '5', label: t`5 uses`},
			{value: '10', label: t`10 uses`},
			{value: '25', label: t`25 uses`},
			{value: '50', label: t`50 uses`},
			{value: '100', label: t`100 uses`},
		],
		[t],
	);

	const [maxAge, setMaxAge] = React.useState('0');
	const [maxUses, setMaxUses] = React.useState('0');
	const [unique, setUnique] = React.useState(false);
	const [inviteCode, setInviteCode] = React.useState<string | null>(null);
	const [copied, setCopied] = React.useState(false);
	const [isCreating, setIsCreating] = React.useState(false);
	const maxAgeSelectId = React.useId();
	const maxUsesSelectId = React.useId();

	const title = type === 'emoji' ? t`Emoji Pack Invite` : t`Sticker Pack Invite`;
	const description =
		type === 'emoji'
			? t`Send a link to let others install your emoji pack. The pack installs automatically when the recipient accepts.`
			: t`Share your sticker pack with others via a simple invite link.`;

	const inviteUrl = inviteCode ? `${RuntimeConfigStore.inviteEndpoint}/${inviteCode}` : '';

	const handleGenerateInvite = async () => {
		setIsCreating(true);
		try {
			const metadata = await PackInviteActionCreators.createInvite({
				packId,
				maxAge: parseInt(maxAge, 10),
				maxUses: parseInt(maxUses, 10),
				unique,
			});
			setInviteCode(metadata.code);
			setCopied(false);
			onCreated?.();
		} finally {
			setIsCreating(false);
		}
	};

	const handleCopy = async () => {
		if (!inviteUrl) return;
		await TextCopyActionCreators.copy(i18n, inviteUrl, true);
		setCopied(true);
		setTimeout(() => setCopied(false), 2000);
	};

	return (
		<Modal.Root size="small" onClose={() => ModalActionCreators.pop()}>
			<Modal.Header title={title} />
			<Modal.Content>
				<p className={styles.description}>{description}</p>

				<div className={styles.fieldGroup}>
					<label htmlFor={maxAgeSelectId} className={styles.fieldLabel}>
						<Trans>Expiration</Trans>
					</label>
					<Select id={maxAgeSelectId} value={maxAge} options={MAX_AGE_OPTIONS} onChange={(value) => setMaxAge(value)} />
				</div>

				<div className={styles.fieldGroup}>
					<label htmlFor={maxUsesSelectId} className={styles.fieldLabel}>
						<Trans>Max uses</Trans>
					</label>
					<Select
						id={maxUsesSelectId}
						value={maxUses}
						options={MAX_USES_OPTIONS}
						onChange={(value) => setMaxUses(value)}
					/>
				</div>

				<div className={styles.fieldGroup}>
					<Switch
						label={<Trans>Unique invite</Trans>}
						value={unique}
						onChange={(value) => setUnique(value)}
						ariaLabel={t`Toggle unique invite`}
					/>
					<p className={styles.helpText}>
						<Trans>Each unique invite can only be used once.</Trans>
					</p>
				</div>

				{inviteCode && (
					<CopyLinkSection
						label={<Trans>Share this link</Trans>}
						value={inviteUrl}
						onCopy={handleCopy}
						copied={copied}
						placeholder={`${RuntimeConfigStore.inviteEndpoint}/...`}
					/>
				)}
			</Modal.Content>
			<Modal.Footer>
				<Button variant="secondary" onClick={() => ModalActionCreators.pop()}>
					<Trans>Close</Trans>
				</Button>
				<Button onClick={handleGenerateInvite} submitting={isCreating}>
					<Trans>Generate Invite</Trans>
				</Button>
			</Modal.Footer>
		</Modal.Root>
	);
});
