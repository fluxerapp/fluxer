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
import {HouseIcon, LinkIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React, {useState} from 'react';
import {useForm} from 'react-hook-form';
import * as GuildActionCreators from '~/actions/GuildActionCreators';
import * as InviteActionCreators from '~/actions/InviteActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {ExternalLink} from '~/components/common/ExternalLink';
import {Form} from '~/components/form/Form';
import {Input} from '~/components/form/Input';
import styles from '~/components/modals/AddGuildModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {Button} from '~/components/uikit/Button/Button';
import {useFormSubmit} from '~/hooks/useFormSubmit';
import {Routes} from '~/Routes';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import * as AvatarUtils from '~/utils/AvatarUtils';
import {openFilePicker} from '~/utils/FilePickerUtils';
import {getInitialsLength} from '~/utils/GuildInitialsUtils';
import * as InviteUtils from '~/utils/InviteUtils';
import * as RouterUtils from '~/utils/RouterUtils';
import * as StringUtils from '~/utils/StringUtils';
import {AssetCropModal, AssetType} from './AssetCropModal';

export type AddGuildModalView = 'landing' | 'create_guild' | 'join_guild';

interface GuildCreateFormInputs {
	icon?: string | null;
	name: string;
}

interface GuildJoinFormInputs {
	code: string;
}

interface ModalFooterContextValue {
	setFooterContent: (content: React.ReactNode) => void;
}

const ModalFooterContext = React.createContext<ModalFooterContextValue | null>(null);

const ActionButton = ({onClick, icon, label}: {onClick: () => void; icon: React.ReactNode; label: string}) => (
	<button type="button" onClick={onClick} className={styles.actionButton}>
		<span className={styles.actionIcon}>{icon}</span>
		<span className={styles.actionLabel}>{label}</span>
	</button>
);

export const AddGuildModal = observer(({initialView = 'landing'}: {initialView?: AddGuildModalView} = {}) => {
	const {t} = useLingui();
	const [view, setView] = useState<AddGuildModalView>(initialView);
	const [footerContent, setFooterContent] = useState<React.ReactNode>(null);

	const getTitle = () => {
		switch (view) {
			case 'landing':
				return t`Add a Community`;
			case 'create_guild':
				return t`Create a Community`;
			case 'join_guild':
				return t`Join a Community`;
		}
	};

	const contextValue = React.useMemo(
		() => ({
			setFooterContent,
		}),
		[],
	);

	return (
		<ModalFooterContext.Provider value={contextValue}>
			<Modal.Root size="small" centered>
				<Modal.Header title={getTitle()} />

				<Modal.Content className={styles.content}>
					{view === 'landing' && <LandingView onViewChange={setView} />}
					{view === 'create_guild' && <GuildCreateForm />}
					{view === 'join_guild' && <GuildJoinForm />}
				</Modal.Content>

				{footerContent && <Modal.Footer>{footerContent}</Modal.Footer>}
			</Modal.Root>
		</ModalFooterContext.Provider>
	);
});

const LandingView = observer(({onViewChange}: {onViewChange: (view: AddGuildModalView) => void}) => {
	const {t} = useLingui();

	return (
		<div className={styles.landingContainer}>
			<p>
				<Trans>Create a new community or join an existing one.</Trans>
			</p>

			<div className={styles.actionButtons}>
				<ActionButton
					onClick={() => onViewChange('create_guild')}
					icon={<HouseIcon size={24} />}
					label={t`Create Community`}
				/>
				<ActionButton
					onClick={() => onViewChange('join_guild')}
					icon={<LinkIcon size={24} weight="regular" />}
					label={t`Join Community`}
				/>
			</div>
		</div>
	);
});

const GuildCreateForm = observer(() => {
	const {t} = useLingui();
	const [previewIconUrl, setPreviewIconUrl] = React.useState<string | null>(null);
	const form = useForm<GuildCreateFormInputs>({defaultValues: {name: ''}});
	const modalFooterContext = React.useContext(ModalFooterContext);
	const formId = React.useId();

	const guildNamePlaceholders = React.useMemo(
		() => [
			t`The Midnight Gamers`,
			t`Study Buddies United`,
			t`Creative Minds Collective`,
			t`Bookworms Anonymous`,
			t`Artists' Corner`,
			t`Dev Den`,
			t`Band Practice Room`,
			t`Volunteer Heroes`,
			t`Hobby Haven`,
			t`Class of '24`,
			t`Team Alpha`,
			t`Family Reunion`,
			t`Project X`,
			t`Weekend Warriors`,
			t`Movie Night Crew`,
			t`Neighborhood Watch`,
			t`Professional Peers`,
			t`Support Circle`,
			t`Coffee Chat`,
			t`Game Night`,
			t`Study Hall`,
			t`Creative Writing Club`,
			t`Photography Club`,
			t`Music Lovers`,
			t`Fitness Friends`,
			t`Foodie Friends`,
			t`Travel Buddies`,
			t`Movie Club`,
			t`Board Game Night`,
			t`Coding Crew`,
			t`Art Club`,
			t`Book Club`,
			t`Sports Fans`,
			t`Gaming Guild`,
			t`Study Group`,
			t`Work Friends`,
			t`Family Chat`,
			t`Friends Forever`,
			t`The Squad`,
			t`Our Hangout`,
		],
		[],
	);

	const randomPlaceholder = React.useMemo(() => {
		const randomIndex = Math.floor(Math.random() * guildNamePlaceholders.length);
		return guildNamePlaceholders[randomIndex];
	}, [guildNamePlaceholders]);

	const nameValue = form.watch('name');

	const initials = React.useMemo(() => {
		const raw = (nameValue || '').trim();
		if (!raw) return '';
		return StringUtils.getInitialsFromName(raw);
	}, [nameValue]);

	const initialsLength = React.useMemo(() => (initials ? getInitialsLength(initials) : null), [initials]);

	const handleIconUpload = React.useCallback(async () => {
		try {
			const [file] = await openFilePicker({accept: 'image/*'});
			if (!file) return;

			if (file.size > 10 * 1024 * 1024) {
				ToastActionCreators.createToast({
					type: 'error',
					children: t`Icon file is too large. Please choose a file smaller than 10MB.`,
				});
				return;
			}

			if (file.type === 'image/gif') {
				ToastActionCreators.createToast({
					type: 'error',
					children: t`Animated icons are not supported when creating a new community. Please use JPEG, PNG, or WebP.`,
				});
				return;
			}

			const base64 = await AvatarUtils.fileToBase64(file);

			ModalActionCreators.push(
				modal(() => (
					<AssetCropModal
						assetType={AssetType.GUILD_ICON}
						imageUrl={base64}
						sourceMimeType={file.type}
						onCropComplete={(croppedBlob) => {
							const reader = new FileReader();
							reader.onload = () => {
								const croppedBase64 = reader.result as string;
								form.setValue('icon', croppedBase64);
								setPreviewIconUrl(croppedBase64);
								form.clearErrors('icon');
							};
							reader.onerror = () => {
								ToastActionCreators.createToast({
									type: 'error',
									children: t`Failed to process the cropped image. Please try again.`,
								});
							};
							reader.readAsDataURL(croppedBlob);
						}}
						onSkip={() => {
							form.setValue('icon', base64);
							setPreviewIconUrl(base64);
							form.clearErrors('icon');
						}}
					/>
				)),
			);
		} catch {
			ToastActionCreators.createToast({
				type: 'error',
				children: <Trans>That image is invalid. Please try another one.</Trans>,
			});
		}
	}, [form]);

	const onSubmit = React.useCallback(async (data: GuildCreateFormInputs) => {
		const guild = await GuildActionCreators.create({
			icon: data.icon,
			name: data.name,
		});
		ModalActionCreators.pop();
		RouterUtils.transitionTo(Routes.guildChannel(guild.id, guild.system_channel_id || undefined));
	}, []);

	const {handleSubmit, isSubmitting} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'name',
	});

	React.useEffect(() => {
		const isNameEmpty = !nameValue?.trim();

		modalFooterContext?.setFooterContent(
			<>
				<Button onClick={ModalActionCreators.pop} variant="secondary">
					<Trans>Cancel</Trans>
				</Button>
				<Button onClick={handleSubmit} submitting={isSubmitting} disabled={isNameEmpty}>
					<Trans>Create Community</Trans>
				</Button>
			</>,
		);

		return () => modalFooterContext?.setFooterContent(null);
	}, [handleSubmit, isSubmitting, modalFooterContext, nameValue]);

	const handleClearIcon = React.useCallback(() => {
		form.setValue('icon', null);
		setPreviewIconUrl(null);
	}, [form]);

	return (
		<div className={styles.formContainer}>
			<p>
				<Trans>Create a community for you and your friends to chat.</Trans>
			</p>

			<Form form={form} onSubmit={handleSubmit} id={formId} aria-label={t`Create community form`}>
				<div className={styles.iconSection}>
					<div className={styles.iconSectionInner}>
						<div className={styles.iconLabel}>
							<Trans>Community Icon</Trans>
						</div>
						<div className={styles.iconPreview}>
							{previewIconUrl ? (
								<div className={styles.iconImage} style={{backgroundImage: `url(${previewIconUrl})`}} />
							) : (
								<div className={styles.iconPlaceholder} data-initials-length={initialsLength}>
									{initials ? <span className={styles.iconInitials}>{initials}</span> : null}
								</div>
							)}
							<div className={styles.iconActions}>
								<div className={styles.iconButtons}>
									<Button variant="secondary" small={true} onClick={handleIconUpload}>
										{previewIconUrl ? <Trans>Change Icon</Trans> : <Trans>Upload Icon</Trans>}
									</Button>
									{previewIconUrl && (
										<Button variant="secondary" small={true} onClick={handleClearIcon}>
											<Trans>Remove Icon</Trans>
										</Button>
									)}
								</div>
								<div className={styles.iconHint}>
									<Trans>JPEG, PNG, WebP. Max 10MB. Recommended: 512×512px</Trans>
								</div>
							</div>
						</div>
						{form.formState.errors.icon?.message && (
							<p className={styles.iconError}>{form.formState.errors.icon.message}</p>
						)}
					</div>

					<Input
						{...form.register('name')}
						autoFocus={true}
						error={form.formState.errors.name?.message}
						label={t`Community Name`}
						minLength={1}
						maxLength={100}
						name="name"
						placeholder={randomPlaceholder}
						required={true}
						type="text"
					/>
					<p className={styles.guidelines}>
						<Trans>
							By creating a community, you agree to follow and uphold the{' '}
							<ExternalLink href={Routes.guidelines()}>Fluxer Community Guidelines</ExternalLink>.
						</Trans>
					</p>
				</div>
			</Form>
		</div>
	);
});

const GuildJoinForm = observer(() => {
	const {t, i18n} = useLingui();
	const form = useForm<GuildJoinFormInputs>({defaultValues: {code: ''}});
	const modalFooterContext = React.useContext(ModalFooterContext);
	const formId = React.useId();

	const randomInviteCode = React.useMemo(() => {
		const chars = 'abcdefghijklmnopqrstuvwxyz0123456789';
		const length = Math.floor(Math.random() * 7) + 6;
		let result = '';
		for (let i = 0; i < length; i++) {
			result += chars.charAt(Math.floor(Math.random() * chars.length));
		}
		return result;
	}, []);

	const onSubmit = React.useCallback(
		async (data: GuildJoinFormInputs) => {
			const parsedCode = InviteUtils.findInvite(data.code) ?? data.code;
			const invite = await InviteActionCreators.fetch(parsedCode);
			await InviteActionCreators.acceptAndTransitionToChannel(invite.code, i18n);
			ModalActionCreators.pop();
		},
		[i18n],
	);

	const {handleSubmit, isSubmitting} = useFormSubmit({
		form,
		onSubmit,
		defaultErrorField: 'code',
	});

	const codeValue = form.watch('code');

	React.useEffect(() => {
		const isCodeEmpty = !codeValue?.trim();

		modalFooterContext?.setFooterContent(
			<>
				<Button onClick={ModalActionCreators.pop} variant="secondary">
					<Trans>Cancel</Trans>
				</Button>
				<Button onClick={handleSubmit} submitting={isSubmitting} disabled={isCodeEmpty}>
					<Trans>Join Community</Trans>
				</Button>
			</>,
		);

		return () => modalFooterContext?.setFooterContent(null);
	}, [handleSubmit, isSubmitting, modalFooterContext, codeValue]);

	return (
		<div className={styles.formContainer}>
			<p>
				<Trans>Enter the invite link to join a community.</Trans>
			</p>

			<Form form={form} onSubmit={handleSubmit} id={formId} aria-label={t`Join community form`}>
				<div className={styles.iconSection}>
					<Input
						{...form.register('code')}
						autoFocus={true}
						error={form.formState.errors.code?.message}
						label={t`Invite Link`}
						minLength={1}
						maxLength={100}
						name="code"
						placeholder={`${RuntimeConfigStore.inviteEndpoint}/${randomInviteCode}`}
						required={true}
						type="text"
					/>
				</div>
			</Form>
		</div>
	);
});
