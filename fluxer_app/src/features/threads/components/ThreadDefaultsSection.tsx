// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsControlRow} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/SettingsControlRow';
import {SlowmodeControl} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/SlowmodeControl';
import type {FormInputs} from '@app/features/channel/components/modals/channel_tabs/channel_overview_tab/shared';
import {getAutoArchiveOptions} from '@app/features/threads/utils/ThreadAutoArchiveOptions';
import * as D from '@app/features/threads/utils/ThreadMessageDescriptors';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import {useLingui} from '@lingui/react/macro';
import {Controller, type UseFormReturn} from 'react-hook-form';

export const ThreadDefaultsSection = ({form}: {form: UseFormReturn<FormInputs>}) => {
	const {i18n} = useLingui();
	return (
		<>
			<SettingsControlRow
				label={i18n._(D.DEFAULT_HIDE_AFTER_INACTIVITY_DESCRIPTOR)}
				description={i18n._(D.HIDE_AFTER_INACTIVITY_HINT_DESCRIPTOR)}
				stacked
				dataFlx="threads.thread-defaults-section.auto-archive"
				data-flx="threads.thread-defaults-section.auto-archive.settings-control-row"
			>
				<Controller
					name="default_auto_archive_duration"
					control={form.control}
					render={({field}) => (
						<RadioGroup
							aria-label={i18n._(D.DEFAULT_HIDE_AFTER_INACTIVITY_DESCRIPTOR)}
							value={field.value ?? null}
							onChange={field.onChange}
							options={getAutoArchiveOptions(i18n)}
							data-flx="threads.thread-defaults-section.radio-group"
						/>
					)}
					data-flx="threads.thread-defaults-section.controller"
				/>
			</SettingsControlRow>
			<SlowmodeControl
				form={form}
				name="default_thread_rate_limit_per_user"
				label={i18n._(D.DEFAULT_THREAD_SLOWMODE_DESCRIPTOR)}
				data-flx="threads.thread-defaults-section.slowmode-control"
			/>
		</>
	);
};
