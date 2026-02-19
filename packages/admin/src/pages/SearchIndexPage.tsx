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

/** @jsxRuntime automatic */
/** @jsxImportSource hono/jsx */

import {type ApiError, isNotFound} from '@fluxer/admin/src/api/Errors';
import {getIndexRefreshStatus} from '@fluxer/admin/src/api/Search';
import {ErrorCard} from '@fluxer/admin/src/components/ErrorDisplay';
import {Layout} from '@fluxer/admin/src/components/Layout';
import {HStack} from '@fluxer/admin/src/components/ui/Layout/HStack';
import {PageLayout} from '@fluxer/admin/src/components/ui/Layout/PageLayout';
import {VStack} from '@fluxer/admin/src/components/ui/Layout/VStack';
import {Heading, Text} from '@fluxer/admin/src/components/ui/Typography';
import type {Session} from '@fluxer/admin/src/types/App';
import type {AdminConfig as Config} from '@fluxer/admin/src/types/Config';
import {formatTimestamp} from '@fluxer/date_utils/src/DateFormatting';
import type {Flash} from '@fluxer/hono/src/Flash';
import type {IndexRefreshStatusResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {UserAdminResponse} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import {Button} from '@fluxer/ui/src/components/Button';
import {Card} from '@fluxer/ui/src/components/Card';
import {CsrfInput} from '@fluxer/ui/src/components/CsrfInput';
import type {FC} from 'hono/jsx';

export interface SearchIndexPageProps {
	config: Config;
	session: Session;
	currentAdmin: UserAdminResponse | undefined;
	flash: Flash | undefined;
	jobId?: string;
	assetVersion: string;
	csrfToken: string;
}

function formatStatusLabel(status: string): string {
	switch (status) {
		case 'in_progress':
			return 'In progress';
		case 'completed':
			return 'Completed';
		case 'failed':
			return 'Failed';
		default:
			return 'Unknown';
	}
}

function formatTimestampLocal(timestamp: string): string {
	return formatTimestamp(timestamp, 'en-US', {
		year: 'numeric',
		month: 'numeric',
		day: 'numeric',
		hour: 'numeric',
		minute: '2-digit',
	});
}

const ReindexButton: FC<{config: Config; title: string; indexType: string; csrfToken: string}> = ({
	config,
	title,
	indexType,
	csrfToken,
}) => (
	<form method="post" action={`${config.basePath}/search-index?action=reindex`}>
		<CsrfInput token={csrfToken} />
		<input type="hidden" name="index_type" value={indexType} />
		<Button type="submit" variant="secondary" fullWidth>
			Reindex {title}
		</Button>
	</form>
);

const DisabledReindexButton: FC<{title: string}> = ({title}) => (
	<VStack>
		<Button type="button" variant="secondary" fullWidth disabled>
			Reindex {title}
		</Button>
	</VStack>
);

const ReindexControls: FC<{config: Config; csrfToken: string}> = ({config, csrfToken}) => (
	<Card padding="md">
		<VStack gap={3}>
			<Heading level={3} class="subtitle text-neutral-900">
				Global Search Indexes
			</Heading>
			<ReindexButton config={config} title="Users" indexType="users" csrfToken={csrfToken} />
			<ReindexButton config={config} title="Guilds" indexType="guilds" csrfToken={csrfToken} />
			<ReindexButton config={config} title="Reports" indexType="reports" csrfToken={csrfToken} />
			<ReindexButton config={config} title="Audit Logs" indexType="audit_logs" csrfToken={csrfToken} />
			<Heading level={3} class="subtitle mt-6 text-neutral-900">
				Discovery Index
			</Heading>
			<Text color="muted" size="sm" class="mb-3">
				Rebuilds the discovery search index for all approved discoverable communities. This syncs guild metadata,
				descriptions, categories, and online counts.
			</Text>
			<ReindexButton config={config} title="Discovery Index" indexType="discovery" csrfToken={csrfToken} />
			<Heading level={3} class="subtitle mt-6 text-neutral-900">
				Guild-specific Search Indexes
			</Heading>
			<Text color="muted" size="sm" class="mb-3">
				These indexes require a guild ID and can only be triggered from the guild detail page.
			</Text>
			<DisabledReindexButton title="Channel Messages" />
		</VStack>
	</Card>
);

interface IndexRefreshStatusWithDetails {
	status: string;
	total?: number;
	indexed?: number;
	started_at?: string;
	completed_at?: string;
	error?: string;
}

const StatusContent: FC<{status: IndexRefreshStatusWithDetails}> = ({status}) => {
	const percentage =
		status.status === 'in_progress' && status.total !== undefined && status.indexed !== undefined
			? status.total === 0
				? 0
				: Math.floor((status.indexed * 100) / status.total)
			: 0;

	return (
		<VStack gap={3}>
			<Text size="sm" class="text-neutral-700">
				Status: {formatStatusLabel(status.status)}
			</Text>
			{status.status === 'in_progress' && status.total !== undefined && status.indexed !== undefined && (
				<VStack gap={2}>
					<HStack justify="between">
						<Text size="sm" class="text-neutral-700">
							{status.indexed} / {status.total} ({percentage}%)
						</Text>
					</HStack>
					<div class="h-2 w-full overflow-hidden rounded-full bg-neutral-200">
						<div class="h-2 bg-neutral-900 transition-[width] duration-300" style={`width: ${percentage}%`} />
					</div>
				</VStack>
			)}
			{status.status === 'completed' && status.total !== undefined && status.indexed !== undefined && (
				<Text size="sm" class="text-neutral-700">
					Indexed {status.indexed} / {status.total} items
				</Text>
			)}
			{status.started_at && (
				<Text size="xs" class="caption text-neutral-500">
					Started {formatTimestampLocal(status.started_at)}
				</Text>
			)}
			{status.completed_at && (
				<Text size="xs" class="caption text-neutral-500">
					Completed {formatTimestampLocal(status.completed_at)}
				</Text>
			)}
			{status.error && (
				<Text size="sm" color="danger">
					{status.error}
				</Text>
			)}
		</VStack>
	);
};

function getErrorDetails(error: ApiError): {title: string; message: string} {
	switch (error.type) {
		case 'unauthorized':
			return {title: 'Authentication Required', message: 'Your session has expired. Please log in again.'};
		case 'forbidden':
			return {title: 'Permission Denied', message: error.message};
		case 'notFound':
			return {title: 'Not Found', message: 'Status information not found.'};
		case 'serverError':
			return {title: 'Server Error', message: 'An internal server error occurred. Please try again later.'};
		case 'networkError':
			return {title: 'Network Error', message: 'Could not connect to the API. Please try again later.'};
		case 'badRequest':
			return {title: 'Validation Error', message: error.message};
		case 'clientError':
			return {title: 'Client Error', message: error.message};
		case 'parseError':
			return {title: 'Response Error', message: 'The server returned an invalid response.'};
		case 'rateLimited':
			return {title: 'Rate Limited', message: `Rate limited. Retry after ${error.retryAfter} seconds.`};
	}
}

const StatusError: FC<{error: ApiError}> = ({error}) => {
	const {title, message} = getErrorDetails(error);
	return <ErrorCard title={title} message={message} />;
};

const StatusSection: FC<{
	config: Config;
	jobId: string;
	statusResult: {ok: true; data: IndexRefreshStatusResponse} | {ok: false; error: ApiError};
}> = ({config, jobId: _jobId, statusResult}) => (
	<Card padding="md">
		<VStack gap={3}>
			<HStack align="center" justify="between">
				<Heading level={2} class="subtitle text-neutral-900">
					Reindex progress
				</Heading>
				<a
					href={`${config.basePath}/search-index`}
					class="text-brand-primary text-sm hover:text-[color-mix(in_srgb,var(--brand-primary)_80%,black)] hover:underline"
				>
					Clear
				</a>
			</HStack>
			{statusResult.ok ? (
				statusResult.data.status === 'not_found' ? (
					<Text size="sm" class="text-neutral-700">
						Preparing job... check back in a moment.
					</Text>
				) : (
					<StatusContent status={statusResult.data} />
				)
			) : isNotFound(statusResult.error) ? (
				<Text size="sm" class="text-neutral-700">
					Preparing job... check back in a moment.
				</Text>
			) : (
				<StatusError error={statusResult.error} />
			)}
		</VStack>
	</Card>
);

export async function SearchIndexPage({
	config,
	session,
	currentAdmin,
	flash,
	jobId,
	assetVersion,
	csrfToken,
}: SearchIndexPageProps) {
	let shouldAutoRefresh = false;
	let statusResult: {ok: true; data: IndexRefreshStatusResponse} | {ok: false; error: ApiError} | null = null;

	if (jobId) {
		statusResult = await getIndexRefreshStatus(config, session, jobId);
		if (statusResult.ok) {
			shouldAutoRefresh = statusResult.data.status === 'in_progress' || statusResult.data.status === 'not_found';
		} else if (isNotFound(statusResult.error)) {
			shouldAutoRefresh = true;
		}
	}

	return (
		<Layout
			csrfToken={csrfToken}
			title="Search Management"
			activePage="search-index"
			config={config}
			session={session}
			currentAdmin={currentAdmin}
			flash={flash}
			autoRefresh={shouldAutoRefresh}
			assetVersion={assetVersion}
		>
			<PageLayout maxWidth="3xl">
				<VStack gap={6}>
					<Heading level={1}>Search Index Management</Heading>
					<ReindexControls config={config} csrfToken={csrfToken} />
					{jobId && statusResult && <StatusSection config={config} jobId={jobId} statusResult={statusResult} />}
				</VStack>
			</PageLayout>
		</Layout>
	);
}
