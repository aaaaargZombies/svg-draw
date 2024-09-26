import "./style.css";
import { Elm } from "./src/Main.elm";

if (process.env.NODE_ENV === "development") {
  const ElmDebugTransform = await import("elm-debug-transformer");

  ElmDebugTransform.register({
    simple_mode: true,
  });
}

const root = document.querySelector("#app div");
const app = Elm.Main.init({ node: root });

app.ports.logPort.subscribe((event) => {
  console.log(event);
});

app.ports.printPort.subscribe(async (_msg) => {
  // annoying hacky thing because Vite loads the styles differently to get HMR
  let styles = "";
  if (process.env.NODE_ENV === "development") {
    styles = document.querySelector("style").innerHTML;
  } else {
    const href = document
      .querySelector('link[rel="stylesheet"]')
      .getAttribute("href");

    styles = await fetch(href)
      .then((res) => res.text())
      .catch((_error) => "");
  }

  const styleNode = document.createElement("style");
  styleNode.innerHTML = styles;

  const serializer = new XMLSerializer();
  const svg = document.querySelector("#SVG");

  svg.prepend(styleNode);

  const serializedSvg = serializer.serializeToString(svg);
  // prep to save to file
  console.log(serializedSvg);
});
