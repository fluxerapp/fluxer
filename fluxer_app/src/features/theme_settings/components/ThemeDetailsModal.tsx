// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {CANCEL_DESCRIPTOR, DESCRIPTION_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import ThemeLibrary, {getThemeLabel} from '@app/features/theme/state/ThemeLibrary';
import {upsertThemeCssHeader} from '@app/features/theme/utils/ThemeCssUtils';
import styles from '@app/features/theme_settings/ThemesTab.module.css';
import {getLinkedThemeFileLine, isLinkedThemeReadOnly} from '@app/features/theme_settings/utils/LinkedThemeFile';
import {
	showThemeSettingsErrorModal,
	WE_COULDN_T_SAVE_THAT_THEME_DESCRIPTOR,
} from '@app/features/theme_settings/utils/ThemeSettingsErrorModal';
import {THEME_SAVED_DESCRIPTOR} from '@app/features/theme_settings/utils/ThemeSettingsMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Input} from '@app/features/ui/components/form/FormInput';
import {parseTagInput} from '@app/features/user/components/modals/tabs/appearance_tab/theme/ThemeUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';

const EDIT_THEME_DESCRIPTOR = msg({
	message: 'Edit {themeName}',
	comment:
		'Title of the modal in Settings > Themes for changing the name, author, version, tags and description of a theme.',
});
const NAME_DESCRIPTOR = msg({
	message: 'Name',
	comment: 'Field label in the theme details modal in Settings > Themes.',
});
const AUTHOR_DESCRIPTOR = msg({
	message: 'Author',
	comment: 'Field label in the theme details modal in Settings > Themes.',
});
const VERSION_DESCRIPTOR = msg({
	message: 'Version',
	comment: 'Field label in the theme details modal in Settings > Themes.',
});
const TAGS_DESCRIPTOR = msg({
	message: 'Tags',
	comment: 'Field label in the theme details modal in Settings > Themes.',
});
const COMMA_SEPARATED_DESCRIPTOR = msg({
	message: 'comma, separated',
	comment: 'Placeholder for the tags field in the theme details modal. Shows that tags are separated by commas.',
});

interface ThemeDetailsModalProps {
	themeId: string;
}

export const ThemeDetailsModal = observer(function ThemeDetailsModal({themeId}: ThemeDetailsModalProps) {
	const {i18n} = useLingui();
	const theme = ThemeLibrary.themes.find((candidate) => candidate.id === themeId) ?? null;
	const [name, setName] = useState(theme?.name ?? '');
	const [author, setAuthor] = useState(theme?.author ?? '');
	const [version, setVersion] = useState(theme?.version ?? '');
	const [tags, setTags] = useState(theme?.tags.join(', ') ?? '');
	const [description, setDescription] = useState(theme?.description ?? '');
	const [saving, setSaving] = useState(false);
	if (!theme) {
		return null;
	}
	const readOnly = isLinkedThemeReadOnly(theme);
	const linkedLine = getLinkedThemeFileLine(i18n, theme);
	const handleSave = () => {
		const metadata = {
			name: name.trim() || theme.name,
			description,
			author,
			version,
			tags: parseTagInput(tags),
		};
		setSaving(true);
		void ThemeLibrary.updateThemeDetails(theme.id, {...metadata, css: upsertThemeCssHeader(theme.css, metadata)})
			.then(() => {
				ToastCommands.success(i18n._(THEME_SAVED_DESCRIPTOR));
				ModalCommands.pop();
			})
			.catch(() =>
				showThemeSettingsErrorModal(
					i18n,
					() => i18n._(WE_COULDN_T_SAVE_THAT_THEME_DESCRIPTOR),
					'theme-settings.theme-details-modal.save-error-modal',
				),
			)
			.finally(() => setSaving(false));
	};
	const handleUnlink = () => {
		void ThemeLibrary.unlinkTheme(theme.id);
	};
	return (
		<Modal.Root size="small" centered data-flx="theme-settings.theme-details-modal.modal-root">
			<Modal.Header
				title={i18n._(EDIT_THEME_DESCRIPTOR, {themeName: getThemeLabel(theme)})}
				data-flx="theme-settings.theme-details-modal.modal-header"
			/>
			<Modal.Content data-flx="theme-settings.theme-details-modal.modal-content">
				<Modal.ContentLayout data-flx="theme-settings.theme-details-modal.modal-content-layout">
					{linkedLine ? (
						<div className={styles.stack} data-flx="theme-settings.theme-details-modal.stack">
							<p className={styles.note} data-flx="theme-settings.theme-details-modal.note">
								{linkedLine}
							</p>
							<div data-flx="theme-settings.theme-details-modal.div">
								<Button
									variant="secondary"
									small
									onClick={handleUnlink}
									data-flx="theme-settings.theme-details-modal.button.unlink"
								>
									<Trans comment="Button that stops a theme following its CSS file on disk and turns it back into an editable copy.">
										Unlink
									</Trans>
								</Button>
							</div>
						</div>
					) : null}
					<Modal.InputGroup data-flx="theme-settings.theme-details-modal.modal-input-group">
						<Input
							label={i18n._(NAME_DESCRIPTOR)}
							value={name}
							readOnly={readOnly}
							onChange={(event) => setName(event.target.value)}
							data-flx="theme-settings.theme-details-modal.input.set-name"
						/>
						<Input
							label={i18n._(AUTHOR_DESCRIPTOR)}
							value={author}
							readOnly={readOnly}
							onChange={(event) => setAuthor(event.target.value)}
							data-flx="theme-settings.theme-details-modal.input.set-author"
						/>
						<Input
							label={i18n._(VERSION_DESCRIPTOR)}
							value={version}
							readOnly={readOnly}
							onChange={(event) => setVersion(event.target.value)}
							data-flx="theme-settings.theme-details-modal.input.set-version"
						/>
						<Input
							label={i18n._(TAGS_DESCRIPTOR)}
							value={tags}
							placeholder={i18n._(COMMA_SEPARATED_DESCRIPTOR)}
							readOnly={readOnly}
							onChange={(event) => setTags(event.target.value)}
							data-flx="theme-settings.theme-details-modal.input.set-tags"
						/>
						<Input
							label={i18n._(DESCRIPTION_DESCRIPTOR)}
							value={description}
							readOnly={readOnly}
							onChange={(event) => setDescription(event.target.value)}
							data-flx="theme-settings.theme-details-modal.input.set-description"
						/>
					</Modal.InputGroup>
				</Modal.ContentLayout>
			</Modal.Content>
			{readOnly ? (
				<Modal.DismissFooter data-flx="theme-settings.theme-details-modal.modal-dismiss-footer" />
			) : (
				<Modal.Footer data-flx="theme-settings.theme-details-modal.modal-footer">
					<Button
						variant="secondary"
						onClick={ModalCommands.pop}
						data-flx="theme-settings.theme-details-modal.button.pop"
					>
						{i18n._(CANCEL_DESCRIPTOR)}
					</Button>
					<Button onClick={handleSave} submitting={saving} data-flx="theme-settings.theme-details-modal.button.save">
						<Trans>Save</Trans>
					</Button>
				</Modal.Footer>
			)}
		</Modal.Root>
	);
});
