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

import {useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon, CheckIcon, CopyIcon} from '@phosphor-icons/react';
import type React from 'react';
import {Input} from '~/components/form/Input';
import {Button} from '~/components/uikit/Button/Button';
import styles from './ApplicationDetail.module.css';

interface ApplicationHeaderProps {
	name: string;
	applicationId: string;
	onBack: () => void;
	onCopyId: () => void;
	idCopied: boolean;
}

export const ApplicationHeader: React.FC<ApplicationHeaderProps> = ({
	name,
	applicationId,
	onBack,
	onCopyId,
	idCopied,
}) => {
	const {t} = useLingui();
	return (
		<div className={styles.pageHeader}>
			<div className={styles.breadcrumbRow}>
				<Button variant="secondary" onClick={onBack} leftIcon={<ArrowLeftIcon size={16} weight="bold" />} fitContent>
					{t`Back to Applications`}
				</Button>
			</div>

			<div className={styles.heroCard}>
				<div className={styles.heroTop}>
					<div>
						<h2 className={styles.heroTitle}>{name}</h2>
						<Input
							label={t`Application ID`}
							value={applicationId}
							readOnly
							className={styles.metaInput}
							rightElement={
								<Button
									variant="secondary"
									compact
									fitContent
									onClick={onCopyId}
									leftIcon={idCopied ? <CheckIcon size={14} weight="bold" /> : <CopyIcon size={14} />}
								>
									{idCopied ? t`Copied` : t`Copy ID`}
								</Button>
							}
						/>
					</div>
				</div>
			</div>
		</div>
	);
};
