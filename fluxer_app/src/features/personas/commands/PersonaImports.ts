import {Endpoints} from '@app/features/app/constants/Endpoints';
import {http} from '@app/features/platform/transport/RestTransport';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {AVATAR_MAX_SIZE} from '@fluxer/constants/src/LimitConstants.js';
import type {PersonaCreateRequest as NewPersona} from '@fluxer/schema/src/domains/persona/PersonaSchemas.js';
import type {PersonaImportModalProps} from '../components/modals/PersonaImportModal';
import type {FishingBucketExport, PluralkitSystem, TupperboxExport} from '../models/PersonaImports';

const logger = new Logger('PersonaImports');

// TODO: make sure the avatar URLs output here get converted!!
export function parseFile(rawJson: string): Array<NewPersona> {
	logger.trace('Starting import parse');
	const jsonData = JSON.parse(rawJson);
	if ('tuppers' in jsonData) {
		logger.info('Starting Tupperbox import');
		const system = jsonData as TupperboxExport;
		return system.tuppers
			.map(
				(member) =>
					({
						internal_name: member.name,
						display_name: member.nick || member.name,
						bio: member.description || null,
						avatar: member.avatar_url || null,
						banner: member.banner || null,
						triggers: member.brackets
							? [
									{
										prefix: member.brackets[0] || null,
										suffix: member.brackets[1] || null,
									},
								]
							: [],
					}) as NewPersona,
			)
			.filter((v) => v);
	} else if ('members' in jsonData) {
		logger.info('Starting Pluralkit import');
		const system = jsonData as PluralkitSystem;
		return system.members
			.map(
				(member) =>
					({
						internal_name: member.name,
						display_name: member.display_name || member.name,
						bio: member.description || member.bio || null,
						avatar: member.avatar_url || null,
						banner: member.banner || null,
						pronouns: member.pronouns || null,
						accent_color: typeof member.color === 'string' ? parseInt(member.color.replace('#', ''), 16) : member.color,
						triggers:
							member.proxy_tags?.map((t) => ({
								prefix: t.prefix || null,
								suffix: t.suffix || null,
							})) || [],
					}) as NewPersona,
			)
			.filter((v) => v);
	} else if ('proxies' in jsonData) {
		logger.info('Starting Fishing Bucket import');
		const system = jsonData as FishingBucketExport;
		return system.proxies
			.map(
				(member) =>
					({
						internal_name: member.name,
						display_name: member.nickname || member.name,
						bio: member.description || null,
						avatar: member.avatar_url || null,
						pronouns: member.pronouns || null,
						triggers:
							member.triggers
								?.map((t) => {
									const split = t.split('{}', 2);
									if (split.length !== 2) return null;
									return {
										prefix: split[0] || null,
										suffix: split[1] || null,
									};
								})
								.filter((v) => v) || [],
					}) as NewPersona,
			)
			.filter((v) => v);
	} else {
		throw new Error('Invalid format. Expected a JSON file as formatted by Pluralkit, Tupperbox, or Fishing Bucket.');
	}
}

interface PrepareStaticImageProps {
	image: Blob;
	maxBytes?: number;
	width?: number;
	height?: number;
}

async function prepareStaticImage({image, maxBytes, ...props}: PrepareStaticImageProps): Promise<Blob> {
	const imageBitmap = await createImageBitmap(image);
	const canvas = new OffscreenCanvas(
		props.width || props.height ? (props.width ?? props.height!) : imageBitmap.width,
		props.width || props.height ? (props.height ?? props.width!) : imageBitmap.height,
	);
	const cctx = canvas.getContext('2d');
	cctx?.drawImage(imageBitmap, 0, 0, canvas.width, canvas.height);
	return await canvas.convertToBlob({type: 'image/png'});
}

async function prepareMedia(
	inputs: Array<NewPersona>,
	progressCallback?: (index: number) => void,
): Promise<Array<NewPersona>> {
	return (
		await Promise.allSettled<NewPersona>(
			inputs.map(async (persona, i): Promise<NewPersona> => {
				let newAvatar: string | undefined;
				let newBanner: string | undefined;
				if (persona.avatar?.match(/^https?[:]\/\//)) {
					const avatarFile = await http
						.get<Blob>(Endpoints.USER_PERSONA_AVATAR, {
							query: {
								url: persona.avatar,
							},
							parse: 'binary',
							mode: 'silent',
						})
						.catch((e) => {
							logger.warn('Failed to fetch avatar', e);
							return null;
						});
					if (avatarFile?.ok && avatarFile.body?.size) {
						const reader = new FileReader();
						const blob = await prepareStaticImage({
							image: avatarFile.body as Blob,
							height: 256,
						});
						if (blob.size > AVATAR_MAX_SIZE) {
							logger.warn(
								`Image is too large (${blob.size} > ${AVATAR_MAX_SIZE}) for '${persona.internal_name}' -- dimensions need to be reduced!`,
							);
						}
						reader.readAsDataURL(blob as Blob);
						/* flush */ await new Promise((resolve, reject) => {
							reader.addEventListener('loadend', resolve);
							reader.addEventListener('error', reject);
							reader.addEventListener('abort', reject);
						});
						newAvatar = String(reader.result);
					} else {
						logger.warn('Failed to look up avatar', avatarFile?.statusText, avatarFile?.text);
						persona.avatar = null;
					}
				}
				if (persona.banner?.match(/^https?[:]\/\//)) {
					const bannerFile = await http
						.get<Blob>(Endpoints.USER_PERSONA_AVATAR, {
							query: {
								url: persona.banner,
							},
							parse: 'binary',
						})
						.catch((e) => {
							logger.warn('Failed to fetch banner', e);
							return null;
						});
					if (bannerFile?.ok && bannerFile?.body.size) {
						const reader = new FileReader();
						const blob = await prepareStaticImage({
							image: bannerFile.body as Blob,
							height: 256,
						});
						if (blob.size > AVATAR_MAX_SIZE) {
							logger.warn(
								`Image is too large (${blob.size} > ${AVATAR_MAX_SIZE}) for '${persona.internal_name}' -- dimensions need to be reduced!`,
							);
						}
						reader.readAsDataURL(blob as Blob);
						/* flush */ await new Promise((resolve, reject) => {
							reader.addEventListener('loadend', resolve);
							reader.addEventListener('error', reject);
							reader.addEventListener('abort', reject);
						});
						newBanner = String(reader.result);
					} else {
						logger.warn('Failed to look up banner', bannerFile?.statusText, bannerFile?.text);
						persona.banner = null;
					}
				}
				void progressCallback?.(i);
				return {...persona, avatar: newAvatar ?? persona.avatar, banner: newBanner ?? persona.banner};
			}),
		)
	).map((v, i) => {
		if (v.status === 'fulfilled') return v.value;
		logger.warn("Couldn't prepare media for imported persona", v.reason);
		return inputs[i];
	});
}

async function uploadImport(inputs: Array<NewPersona>): Promise<void> {
	await http.post(Endpoints.USER_PERSONAS_BATCH, {body: inputs});
}

export async function processImport(
	data: string,
	setImportState: (
		setter: PersonaImportModalProps | ((state: PersonaImportModalProps) => PersonaImportModalProps),
	) => void,
	markUpdated: () => void,
	stopUpdating: () => void,
) {
	try {
		const imported = parseFile(data);
		console.log(imported);

		setImportState((state) => ({...state, stage: 'FETCHING_MEDIA', max: imported.length, value: 0}));
		const assets_updated = await prepareMedia(imported, (i) =>
			setImportState((state) => {
				if (state.stage === 'FETCHING_MEDIA' && state.value < i) state.value = i + 1;
				return state;
			}),
		);

		let filtered: typeof imported;
		await new Promise<void>((resolve, _) => {
			setImportState((state) => ({
				...state,
				stage: 'PENDING_FILTER',
				filterInput: assets_updated,
				onFiltered: (v) => {
					filtered = v;
					resolve();
				},
			}));
		});
		//@ts-expect-error(2454): this is always set before the promise above resolves. it may also just be empty
		if (!filtered?.length) {
			setImportState((state) => ({...state, stage: 'DONE', max: 0, value: 0}));
			return;
		}

		setImportState((state) => ({...state, stage: 'UPLOADING'}));
		// TODO: blocking loading modal while import runs
		await uploadImport(filtered);
		setImportState((state) => ({...state, stage: 'DONE', max: filtered.length, value: filtered.length}));
		markUpdated(); // setImportEpoch((v) => v+1);
	} catch (e: any) {
		console.error('Could not import personas', e);
		if ('body' in e && 'message' in e.body) {
			setImportState((state) => ({
				...state,
				stage: 'ERROR',
				error: {
					code: 'UPLOAD_FAILED',
					message: String(e.body.message),
				},
			}));
		} else {
			setImportState((state) => ({
				...state,
				stage: 'ERROR',
				error: {
					code: 'UPLOAD_FAILED',
					message: String(e),
				},
			}));
		}
	} finally {
		stopUpdating(); // setImporting(false);
	}
}
