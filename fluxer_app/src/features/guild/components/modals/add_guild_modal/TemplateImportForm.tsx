// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import {ExternalLink} from '@app/features/app/components/shared/ExternalLink';
import {
	AVATAR_RECOMMENDED_SIZE_LABEL,
	IMAGE_MAX_SIZE_LABEL,
	PRODUCT_NAME,
	STATIC_IMAGE_FORMATS,
	THE_OTHER_PLATFORM_TEMPLATE_EXAMPLE_URL,
} from '@app/features/app/config/I18nDisplayConstants';
import {useFormSubmit} from '@app/features/app/hooks/useFormSubmit';
import {openClaimAccountModal} from '@app/features/auth/components/modals/ClaimAccountModal';
import {AssetCropModal, AssetType} from '@app/features/expressions/components/modals/AssetCropModal';
import {openAssetSourceModal} from '@app/features/expressions/components/modals/AssetSourceModal';
import {isAnimatedFile} from '@app/features/expressions/utils/AnimatedImageUtils';
import {getAcceptStringFiltered} from '@app/features/expressions/utils/AssetFormatCopy';
import {formatImageUploadRecommendedHint} from '@app/features/expressions/utils/AssetUploadHintCopy';
import {isSvgFile, readImageFileAsUploadDataUrl} from '@app/features/expressions/utils/ImageUploadFileUtils';
import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import {showGuildErrorModal} from '@app/features/guild/components/alerts/GuildErrorModalUtils';
import styles from '@app/features/guild/components/modals/AddGuildModal.module.css';
import {
	ANIMATED_ICONS_ARE_NOT_SUPPORTED_WHEN_CREATING_A_DESCRIPTOR,
	COMMUNITY_NAME_DESCRIPTOR,
	CREATE_FROM_TEMPLATE_FORM_DESCRIPTOR,
	handleGuildCreationError,
	ICON_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR,
	IMPORT_TEMPLATE_FORM_DESCRIPTOR,
	INVALID_JSON_PLEASE_PASTE_THE_FULL_RESPONSE_FROM_DESCRIPTOR,
	isTemplateEveryoneRole,
	ModalFooterContext,
	mapTemplateChannelTypeToFluxer,
	PASTE_TEMPLATE_JSON_FORM_DESCRIPTOR,
	PASTE_THE_JSON_RESPONSE_HERE_DESCRIPTOR,
	PLEASE_ENTER_A_VALID_THE_OTHER_PLATFORM_TEMPLATE_URL_OR_DESCRIPTOR,
	parseTemplateCode,
	TEMPLATE_JSON_DESCRIPTOR,
	TEMPLATE_URL_DESCRIPTOR,
	type TemplateCreateFormInputs,
	type TemplateImportFormInputs,
	type TemplateJsonFormInputs,
	THIS_DOESN_T_LOOK_LIKE_A_VALID_TEMPLATE_DESCRIPTOR,
} from '@app/features/guild/components/modals/add_guild_modal/shared';
import {getGuildIconDisplayInitials, getInitialsLength} from '@app/features/guild/utils/GuildInitialsUtils';
import {
	CREATE_COMMUNITY_DESCRIPTOR,
	FAILED_TO_PROCESS_CROPPED_IMAGE_DESCRIPTOR,
	INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR,
	NEXT_DESCRIPTOR,
	VERIFY_EMAIL_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {openExternalUrlWithWarning} from '@app/features/messaging/utils/ExternalLinkUtils';
import {openFilePicker} from '@app/features/messaging/utils/FilePickerUtils';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Form} from '@app/features/ui/components/form/Form';
import {Input, Textarea} from '@app/features/ui/components/form/FormInput';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {UserSettingsModal} from '@app/features/user/components/modals/UserSettingsModal';
import Users from '@app/features/user/state/Users';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as StringUtils from '@app/lib/strings';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {THE_OTHER_PLATFORM} from '@fluxer/constants/src/ExternalPlatformConstants';
import type {TemplateSerializedGuild} from '@fluxer/schema/src/domains/guild/GuildTemplateSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowSquareOutIcon, EnvelopeSimpleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useContext, useEffect, useId, useMemo, useState} from 'react';
import {useForm} from 'react-hook-form';
import * as v from 'valibot';

const TemplateEntityId = v.union([
	v.pipe(v.number(), v.integer(), v.minValue(0)),
	v.pipe(v.string(), v.regex(/^\d+$/)),
]);
const PermissionBitfield = v.union([v.string(), v.pipe(v.number(), v.integer(), v.minValue(0))]);
const CHANGE_ICON_DESCRIPTOR = msg({
	message: 'Change icon',
	comment:
		'Title of the modal where the user picks an imported-template community icon source. Keep it concise. Keep the tone plain and specific.',
});
const OverwriteType = v.pipe(
	v.union([v.number(), v.string()]),
	v.transform((value) => {
		if (typeof value === 'string') {
			if (value === 'role') return 0;
			if (value === 'member') return 1;
			return Number(value);
		}
		return value;
	}),
);
const TemplateChannelSchema = v.object({
	id: TemplateEntityId,
	type: v.number(),
	name: v.nullish(v.string(), ''),
	topic: v.nullish(v.string()),
	position: v.number(),
	parent_id: v.nullish(TemplateEntityId),
	bitrate: v.nullish(v.number()),
	user_limit: v.nullish(v.number()),
	voice_connection_limit: v.nullish(v.number()),
	nsfw: v.optional(v.boolean()),
	rate_limit_per_user: v.optional(v.number()),
	permission_overwrites: v.optional(
		v.array(v.object({id: TemplateEntityId, type: OverwriteType, allow: PermissionBitfield, deny: PermissionBitfield})),
	),
});
const TemplateRoleSchema = v.object({
	id: TemplateEntityId,
	name: v.nullish(v.string(), ''),
	permissions: v.optional(PermissionBitfield),
	permissions_new: v.optional(PermissionBitfield),
	color: v.optional(v.number()),
	hoist: v.optional(v.boolean()),
	mentionable: v.optional(v.boolean()),
	unicode_emoji: v.nullish(v.string()),
});
const GuildTemplateResponseSchema = v.object({
	code: v.string(),
	name: v.string(),
	description: v.nullish(v.string()),
	serialized_source_guild: v.object({
		name: v.string(),
		description: v.nullish(v.string()),
		verification_level: v.optional(v.number()),
		default_message_notifications: v.optional(v.number()),
		explicit_content_filter: v.optional(v.number()),
		system_channel_id: v.nullish(TemplateEntityId),
		afk_timeout: v.optional(v.number()),
		system_channel_flags: v.optional(v.number()),
		roles: v.array(TemplateRoleSchema),
		channels: v.array(TemplateChannelSchema),
	}),
});

type TemplateImportStep = 'url' | 'json' | 'create';

const TEMPLATE_IMPORT_STEP_ORDER: ReadonlyArray<TemplateImportStep> = ['url', 'json', 'create'];
const TEMPLATE_STATS_DESCRIPTOR = msg({
	message: '{textChannelCount} text, {voiceChannelCount} voice, {categoryCount} categories, {roleCount} roles',
	comment:
		'Template preview summary listing how many text channels, voice channels, categories, and roles will be imported.',
});
const IMAGE_COULDN_T_BE_USED_DESCRIPTOR = msg({
	message: "Image couldn't be used",
	comment: 'Error modal title shown when a template-import community icon upload cannot be accepted or processed.',
});
export const TemplateImportForm = observer(() => {
	const {i18n} = useLingui();
	const theOtherPlatform = THE_OTHER_PLATFORM;
	const [step, setStep] = useState<TemplateImportStep>('url');
	const [apiUrl, setApiUrl] = useState<string | null>(null);
	const [templateData, setTemplateData] = useState<TemplateSerializedGuild | null>(null);
	const [templateName, setTemplateName] = useState<string | null>(null);
	const urlForm = useForm<TemplateImportFormInputs>({defaultValues: {url: ''}});
	const jsonForm = useForm<TemplateJsonFormInputs>({defaultValues: {json: ''}});
	const createForm = useForm<TemplateCreateFormInputs>({defaultValues: {name: ''}});
	const [previewIconUrl, setPreviewIconUrl] = useState<string | null>(null);
	const modalFooterContext = useContext(ModalFooterContext);
	const urlFormId = useId();
	const jsonFormId = useId();
	const createFormId = useId();
	const currentUser = Users.currentUser;
	const shouldRequireClaimedAccount = currentUser != null && !currentUser.isClaimed();
	const handleResolveUrl = useCallback(
		(data: TemplateImportFormInputs) => {
			const code = parseTemplateCode(data.url);
			if (!code) {
				urlForm.setError('url', {
					message: i18n._(PLEASE_ENTER_A_VALID_THE_OTHER_PLATFORM_TEMPLATE_URL_OR_DESCRIPTOR, {
						theOtherPlatform: THE_OTHER_PLATFORM,
					}),
				});
				return;
			}
			setApiUrl(`https://discord.com/api/guilds/templates/${code}`);
			setStep('json');
		},
		[urlForm, i18n],
	);
	const {handleSubmit: handleUrlSubmit} = useFormSubmit({
		form: urlForm,
		onSubmit: handleResolveUrl,
		defaultErrorField: 'url',
	});
	const handleParseJson = useCallback(
		(data: TemplateJsonFormInputs) => {
			let json: unknown;
			try {
				json = JSON.parse(data.json);
			} catch {
				jsonForm.setError('json', {message: i18n._(INVALID_JSON_PLEASE_PASTE_THE_FULL_RESPONSE_FROM_DESCRIPTOR)});
				return;
			}
			const parsed = v.safeParse(GuildTemplateResponseSchema, json);
			if (!parsed.success) {
				jsonForm.setError('json', {message: i18n._(THIS_DOESN_T_LOOK_LIKE_A_VALID_TEMPLATE_DESCRIPTOR)});
				return;
			}
			setTemplateData(parsed.output.serialized_source_guild);
			setTemplateName(parsed.output.name);
			createForm.setValue('name', parsed.output.serialized_source_guild.name);
			setStep('create');
		},
		[jsonForm, createForm, i18n],
	);
	const {handleSubmit: handleJsonSubmit} = useFormSubmit({
		form: jsonForm,
		onSubmit: handleParseJson,
		defaultErrorField: 'json',
	});
	const handleCreateFromTemplate = useCallback(
		async (data: TemplateCreateFormInputs) => {
			if (!templateData) return;
			try {
				const guild = await GuildCommands.createFromTemplate({
					name: data.name,
					icon: data.icon,
					template: templateData,
				});
				ModalCommands.pop();
				NavigationCommands.selectChannel(guild.id, guild.system_channel_id || undefined);
			} catch (error) {
				handleGuildCreationError(error);
			}
		},
		[templateData],
	);
	const {handleSubmit: handleCreateSubmit, isSubmitting: isCreating} = useFormSubmit({
		form: createForm,
		onSubmit: handleCreateFromTemplate,
		defaultErrorField: 'name',
	});
	const nameValue = createForm.watch('name');
	const urlValue = urlForm.watch('url');
	const jsonValue = jsonForm.watch('json');
	const rawInitials = useMemo(() => {
		const raw = (nameValue || '').trim();
		if (!raw) return '';
		return StringUtils.getInitialsFromName(raw);
	}, [nameValue]);
	const initials = useMemo(() => getGuildIconDisplayInitials(rawInitials), [rawInitials]);
	const initialsLength = useMemo(() => (rawInitials ? getInitialsLength(rawInitials) : null), [rawInitials]);
	const showIconUploadErrorModal = useCallback(
		(message: string) => {
			showGuildErrorModal({
				title: i18n._(IMAGE_COULDN_T_BE_USED_DESCRIPTOR),
				message,
				dataFlx: 'guild.add-guild-modal.template-import-form.icon-upload-error-modal',
			});
		},
		[i18n],
	);
	const templateStats = useMemo(() => {
		if (!templateData) return null;
		const channelTypes = templateData.channels
			.map((channel) => mapTemplateChannelTypeToFluxer(channel.type))
			.filter((channelType): channelType is number => channelType !== null);
		const textChannels = channelTypes.filter((channelType) => channelType === ChannelTypes.GUILD_TEXT).length;
		const voiceChannels = channelTypes.filter((channelType) => channelType === ChannelTypes.GUILD_VOICE).length;
		const categories = channelTypes.filter((channelType) => channelType === ChannelTypes.GUILD_CATEGORY).length;
		const roles = templateData.roles.filter((role) => !isTemplateEveryoneRole(role)).length;
		return {textChannels, voiceChannels, categories, roles};
	}, [templateData]);
	const handleIconUpload = useCallback(async () => {
		try {
			const [file] = await openFilePicker({accept: getAcceptStringFiltered('guild_icon', false)});
			if (!file) return;
			if (file.size > 10 * 1024 * 1024) {
				showIconUploadErrorModal(
					i18n._(ICON_FILE_IS_TOO_LARGE_PLEASE_CHOOSE_A_DESCRIPTOR, {
						imageMaxSizeLabel: IMAGE_MAX_SIZE_LABEL,
					}),
				);
				return;
			}
			const svg = isSvgFile(file);
			const animated = svg ? false : await isAnimatedFile(file);
			if (animated) {
				showIconUploadErrorModal(i18n._(ANIMATED_ICONS_ARE_NOT_SUPPORTED_WHEN_CREATING_A_DESCRIPTOR));
				return;
			}
			const base64 = svg ? await readImageFileAsUploadDataUrl(file) : await AvatarUtils.fileToBase64(file);
			ModalCommands.push(
				modal(() => (
					<AssetCropModal
						assetType={AssetType.GUILD_ICON}
						imageUrl={base64}
						sourceMimeType={svg ? 'image/svg+xml' : file.type}
						onCropComplete={(croppedBlob) => {
							const reader = new FileReader();
							reader.onload = () => {
								const croppedBase64 = reader.result as string;
								createForm.setValue('icon', croppedBase64);
								setPreviewIconUrl(croppedBase64);
							};
							reader.onerror = () => {
								showIconUploadErrorModal(i18n._(FAILED_TO_PROCESS_CROPPED_IMAGE_DESCRIPTOR));
							};
							reader.readAsDataURL(croppedBlob);
						}}
						onSkip={() => {
							createForm.setValue('icon', base64);
							setPreviewIconUrl(base64);
						}}
						data-flx="guild.add-guild-modal.handle-icon-upload.asset-crop-modal--2"
					/>
				)),
			);
		} catch {
			showIconUploadErrorModal(i18n._(INVALID_IMAGE_TRY_ANOTHER_DESCRIPTOR));
		}
	}, [createForm, i18n, showIconUploadErrorModal]);
	const handleClearIcon = useCallback(() => {
		createForm.setValue('icon', null);
		setPreviewIconUrl(null);
	}, [createForm]);
	const handleOpenIconUpload = useCallback(() => {
		openAssetSourceModal({
			title: i18n._(CHANGE_ICON_DESCRIPTOR),
			uploadHint: formatImageUploadRecommendedHint(i18n, {
				formats: STATIC_IMAGE_FORMATS,
				maxSize: IMAGE_MAX_SIZE_LABEL,
				recommendedSize: AVATAR_RECOMMENDED_SIZE_LABEL,
			}),
			onPickUpload: handleIconUpload,
			showGifOption: false,
		});
	}, [handleIconUpload, i18n]);
	const handleOpenApiUrl = useCallback(() => {
		if (apiUrl) {
			openExternalUrlWithWarning(apiUrl);
		}
	}, [apiUrl]);
	useEffect(() => {
		if (step === 'create') {
			const isNameEmpty = !nameValue?.trim();
			modalFooterContext?.setFooterContent(
				<>
					<Button
						onClick={() => setStep('json')}
						variant="secondary"
						data-flx="guild.add-guild-modal.template-import-form.button.back-to-json"
					>
						<Trans>Back</Trans>
					</Button>
					<Button
						onClick={handleCreateSubmit}
						submitting={isCreating}
						disabled={isNameEmpty}
						data-flx="guild.add-guild-modal.template-import-form.button.create-submit"
					>
						{i18n._(CREATE_COMMUNITY_DESCRIPTOR)}
					</Button>
				</>,
			);
		} else if (step === 'json') {
			const isJsonEmpty = !jsonValue?.trim();
			modalFooterContext?.setFooterContent(
				<>
					<Button
						onClick={() => setStep('url')}
						variant="secondary"
						data-flx="guild.add-guild-modal.template-import-form.button.set-step"
					>
						<Trans>Back</Trans>
					</Button>
					<Button
						onClick={handleJsonSubmit}
						disabled={isJsonEmpty}
						data-flx="guild.add-guild-modal.template-import-form.button.json-submit"
					>
						{i18n._(NEXT_DESCRIPTOR)}
					</Button>
				</>,
			);
		} else {
			const isUrlEmpty = !urlValue?.trim();
			modalFooterContext?.setFooterContent(
				<>
					<Button
						onClick={modalFooterContext?.onBack}
						variant="secondary"
						data-flx="guild.add-guild-modal.template-import-form.button.back-to-landing"
					>
						<Trans>Back</Trans>
					</Button>
					<Button
						onClick={handleUrlSubmit}
						disabled={isUrlEmpty}
						data-flx="guild.add-guild-modal.template-import-form.button.url-submit"
					>
						{i18n._(NEXT_DESCRIPTOR)}
					</Button>
				</>,
			);
		}
		return () => modalFooterContext?.setFooterContent(null);
	}, [
		step,
		handleUrlSubmit,
		handleJsonSubmit,
		handleCreateSubmit,
		isCreating,
		modalFooterContext,
		nameValue,
		urlValue,
		jsonValue,
	]);
	if (shouldRequireClaimedAccount) {
		return (
			<div className={styles.formContainer} data-flx="guild.add-guild-modal.template-import-form.form-container">
				<div
					className={styles.verificationNotice}
					data-flx="guild.add-guild-modal.template-import-form.verification-notice"
				>
					<EnvelopeSimpleIcon
						size={remFromPx(32)}
						weight="fill"
						data-flx="guild.add-guild-modal.template-import-form.envelope-simple-icon"
					/>
					<p data-flx="guild.add-guild-modal.template-import-form.p">
						<Trans>You need to claim your account before you can create a community.</Trans>
					</p>
					<Button
						onClick={() => openClaimAccountModal({force: true})}
						data-flx="guild.add-guild-modal.template-import-form.button.open-claim-account-modal"
					>
						<Trans>Claim your account</Trans>
					</Button>
				</div>
			</div>
		);
	}
	if (currentUser?.verified === false) {
		return (
			<div className={styles.formContainer} data-flx="guild.add-guild-modal.template-import-form.form-container--2">
				<div
					className={styles.verificationNotice}
					data-flx="guild.add-guild-modal.template-import-form.verification-notice--2"
				>
					<EnvelopeSimpleIcon
						size={remFromPx(32)}
						weight="fill"
						data-flx="guild.add-guild-modal.template-import-form.envelope-simple-icon--2"
					/>
					<p data-flx="guild.add-guild-modal.template-import-form.p--2">
						<Trans>You need to verify your email address before you can create a community.</Trans>
					</p>
					<Button
						onClick={() =>
							ModalCommands.push(
								modal(() => (
									<UserSettingsModal
										initialTab="account_security"
										data-flx="guild.add-guild-modal.template-import-form.user-settings-modal"
									/>
								)),
							)
						}
						data-flx="guild.add-guild-modal.template-import-form.button.push"
					>
						{i18n._(VERIFY_EMAIL_DESCRIPTOR)}
					</Button>
				</div>
			</div>
		);
	}
	const renderUrlStep = (): React.ReactNode => (
		<div className={styles.formContainer} data-flx="guild.add-guild-modal.template-import-form.form-container--3">
			<Modal.Description data-flx="guild.add-guild-modal.template-import-form.modal-description">
				<Trans>Paste a {theOtherPlatform} template URL to import its structure into a new community.</Trans>
			</Modal.Description>
			<Form
				form={urlForm}
				onSubmit={handleUrlSubmit}
				id={urlFormId}
				aria-label={i18n._(IMPORT_TEMPLATE_FORM_DESCRIPTOR)}
				data-flx="guild.add-guild-modal.template-import-form.form.url-submit"
			>
				<div className={styles.iconSection} data-flx="guild.add-guild-modal.template-import-form.icon-section">
					<Input
						data-flx="guild.add-guild-modal.template-import-form.input.text"
						{...urlForm.register('url')}
						autoFocus={true}
						error={urlForm.formState.errors.url?.message}
						label={i18n._(TEMPLATE_URL_DESCRIPTOR)}
						name="url"
						placeholder={THE_OTHER_PLATFORM_TEMPLATE_EXAMPLE_URL}
						required={true}
						type="text"
					/>
				</div>
			</Form>
		</div>
	);
	const renderJsonStep = (): React.ReactNode => (
		<div className={styles.formContainer} data-flx="guild.add-guild-modal.template-import-form.form-container--4">
			<Modal.Description data-flx="guild.add-guild-modal.template-import-form.modal-description--2">
				<Trans>
					Open the link below in your browser, then copy the entire JSON response and paste it into the box.
				</Trans>
			</Modal.Description>
			<Button
				variant="secondary"
				onClick={handleOpenApiUrl}
				data-flx="guild.add-guild-modal.template-import-form.button.open-api-url"
			>
				<span
					className={styles.openApiUrlButton}
					data-flx="guild.add-guild-modal.template-import-form.open-api-url-button"
				>
					<ArrowSquareOutIcon
						size={remFromPx(16)}
						data-flx="guild.add-guild-modal.template-import-form.arrow-square-out-icon"
					/>
					<Trans>Open template JSON</Trans>
				</span>
			</Button>
			<Form
				form={jsonForm}
				onSubmit={handleJsonSubmit}
				id={jsonFormId}
				aria-label={i18n._(PASTE_TEMPLATE_JSON_FORM_DESCRIPTOR)}
				data-flx="guild.add-guild-modal.template-import-form.form.json-submit"
			>
				<div className={styles.iconSection} data-flx="guild.add-guild-modal.template-import-form.icon-section--2">
					<Textarea
						data-flx="guild.add-guild-modal.template-import-form.textarea"
						{...jsonForm.register('json')}
						autoFocus={true}
						error={jsonForm.formState.errors.json?.message}
						label={i18n._(TEMPLATE_JSON_DESCRIPTOR)}
						name="json"
						placeholder={i18n._(PASTE_THE_JSON_RESPONSE_HERE_DESCRIPTOR)}
						required={true}
						rows={6}
					/>
				</div>
			</Form>
		</div>
	);
	const renderCreateStep = (): React.ReactNode => (
		<div className={styles.formContainer} data-flx="guild.add-guild-modal.template-import-form.form-container--5">
			{templateName && (
				<div className={styles.templatePreview} data-flx="guild.add-guild-modal.template-import-form.template-preview">
					<div className={styles.templateInfo} data-flx="guild.add-guild-modal.template-import-form.template-info">
						<span className={styles.templateLabel} data-flx="guild.add-guild-modal.template-import-form.template-label">
							<Trans>Template</Trans>
						</span>
						<span className={styles.templateName} data-flx="guild.add-guild-modal.template-import-form.template-name">
							{templateName}
						</span>
					</div>
					{templateStats && (
						<div className={styles.templateStats} data-flx="guild.add-guild-modal.template-import-form.template-stats">
							<span data-flx="guild.add-guild-modal.template-import-form.span">
								{i18n._(TEMPLATE_STATS_DESCRIPTOR, {
									textChannelCount: templateStats.textChannels,
									voiceChannelCount: templateStats.voiceChannels,
									categoryCount: templateStats.categories,
									roleCount: templateStats.roles,
								})}
							</span>
						</div>
					)}
				</div>
			)}
			<Form
				form={createForm}
				onSubmit={handleCreateSubmit}
				id={createFormId}
				aria-label={i18n._(CREATE_FROM_TEMPLATE_FORM_DESCRIPTOR)}
				data-flx="guild.add-guild-modal.template-import-form.form.create-submit"
			>
				<div className={styles.iconSection} data-flx="guild.add-guild-modal.template-import-form.icon-section--3">
					<div
						className={styles.iconSectionInner}
						data-flx="guild.add-guild-modal.template-import-form.icon-section-inner"
					>
						<div className={styles.iconLabel} data-flx="guild.add-guild-modal.template-import-form.icon-label">
							<Trans>Community icon</Trans>
						</div>
						<div className={styles.iconPreview} data-flx="guild.add-guild-modal.template-import-form.icon-preview">
							{previewIconUrl ? (
								<div
									className={styles.iconImage}
									style={{backgroundImage: `url(${previewIconUrl})`}}
									data-flx="guild.add-guild-modal.template-import-form.icon-image"
								/>
							) : (
								<div
									className={styles.iconPlaceholder}
									data-initials-length={initialsLength}
									data-flx="guild.add-guild-modal.template-import-form.icon-placeholder"
								>
									{initials ? (
										<span
											className={styles.iconInitials}
											data-flx="guild.add-guild-modal.template-import-form.icon-initials"
										>
											{initials}
										</span>
									) : null}
								</div>
							)}
							<div className={styles.iconActions} data-flx="guild.add-guild-modal.template-import-form.icon-actions">
								<div className={styles.iconButtons} data-flx="guild.add-guild-modal.template-import-form.icon-buttons">
									<Button
										variant="secondary"
										small={true}
										onClick={handleOpenIconUpload}
										data-flx="guild.add-guild-modal.template-import-form.button.icon-upload"
									>
										{previewIconUrl ? <Trans>Change icon</Trans> : <Trans>Upload icon</Trans>}
									</Button>
									{previewIconUrl && (
										<Button
											variant="secondary"
											small={true}
											onClick={handleClearIcon}
											data-flx="guild.add-guild-modal.template-import-form.button.clear-icon"
										>
											<Trans>Remove icon</Trans>
										</Button>
									)}
								</div>
							</div>
						</div>
					</div>
					<Input
						data-flx="guild.add-guild-modal.template-import-form.input.text--2"
						{...createForm.register('name')}
						autoFocus={true}
						error={createForm.formState.errors.name?.message}
						label={i18n._(COMMUNITY_NAME_DESCRIPTOR)}
						minLength={1}
						maxLength={100}
						name="name"
						required={true}
						type="text"
					/>
					<p className={styles.guidelines} data-flx="guild.add-guild-modal.template-import-form.guidelines">
						<Trans>
							By creating a community, you agree to follow and uphold the{' '}
							<ExternalLink
								href={Routes.guidelines()}
								className={styles.guidelinesLink}
								data-flx="guild.add-guild-modal.template-import-form.guidelines-link"
							>
								{PRODUCT_NAME} community guidelines
							</ExternalLink>
							.
						</Trans>
					</p>
				</div>
			</Form>
		</div>
	);
	const renderStepContent = (): React.ReactNode => {
		switch (step) {
			case 'url':
				return renderUrlStep();
			case 'json':
				return renderJsonStep();
			case 'create':
				return renderCreateStep();
		}
	};
	return (
		<SteppedCarousel
			step={step}
			steps={TEMPLATE_IMPORT_STEP_ORDER}
			data-flx="guild.add-guild-modal.template-import-form.stepped-carousel"
		>
			{renderStepContent()}
		</SteppedCarousel>
	);
});
