import { Controller } from "stimulus";

export default class extends Controller {
  static targets = ["modes", "screenshots"];

  findModeIndex(target) {
    const nodes = Array.from(this.modesTarget.querySelectorAll("li button"));

    return nodes.indexOf(target);

  }

  disableAllModes() {
    const nodes = document.querySelectorAll(".modes li");

    for (let i = 0; i < nodes.length; i++) {
      nodes.item(i).classList = [];
    }
  }

  selectScreenshotFor(index) {
    const nodes = this.screenshotsTarget.querySelectorAll("img");

    for (let i = 0; i < nodes.length; i++) {
      nodes.item(i).classList = i === index ? ["active"] : [];
    }
  }

  handleLangClick({ target }) {
    this.disableAllModes();

    target.parentNode.classList = ["active"];

    const activeIndex = this.findModeIndex(target);

    this.selectScreenshotFor(activeIndex);
  }
}
