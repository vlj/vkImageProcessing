#include "highlevelhelpers.h"
#include <v2utils.h>

HighLevelHelpers::HighLevelHelpers(Renderer &_renderer) : renderer(_renderer), r32fToRgba8Pipeline(*renderer.dev) {}

void HighLevelHelpers::CopyF32ToRGBA8Sync(v2::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eR32Sfloat> &tex,
                                          v2::DecoratedState<vk::ImageLayout::eGeneral, vk::Format::eB8G8R8A8Unorm> &texout) {
  assert(tex.tex->width == texout.tex->width);
  assert(tex.tex->height == texout.tex->height);
  size_t width = tex.tex->width;
  size_t height = tex.tex->height;
  auto cmdbuffer = v2::CreateOneShotStartedBuffer(*renderer.dev, *renderer.commandPool);
  r32fToRgba8Pipeline({size_t((width + 15) / 16), size_t((height + 15) / 16)}, cmdbuffer, *renderer.descriptorSetPool,
                      tex, texout);
  auto endedCmdBuffer = v2::EndBufferRecording(std::move(cmdbuffer));
  auto [fence, bufferToClean] = v2::SubmitBuffer(*renderer.dev, renderer.queue, std::move(endedCmdBuffer));
  v2::WaitAndReset(*renderer.dev, *renderer.descriptorSetPool, *renderer.commandPool, std::move(*fence));
}

